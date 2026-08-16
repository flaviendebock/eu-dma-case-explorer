# =============================================================================
# Clean DMA case-data JSON into tidy tables
# Source: https://data.europa.eu/data/datasets/72358eb1-37aa-40bb-8047-d87154d57ac1
# =============================================================================
#
# Key issue this script fixes:
# The raw JSON stores every field as a length-1 (or length-0) ARRAY, and some
# fields are themselves JSON *encoded as a string* one level down (e.g.
# caseOrigin = "{\"code\":...,\"label\":...}"). Reading this with the default
# fromJSON() auto-simplifies inconsistently and produces the messy, hard-to-
# filter output you get from `fromJSON("DMA/case-data-DMA.json")`.
#
# Instead, we read with simplifyVector = FALSE (keeps the true nested list
# structure) and walk the tree explicitly with purrr, producing four tidy,
# linked tables:
#   cases_df        - one row per case
#   decisions_df     - one row per decision (linked to cases_df by case_number)
#   case_attachments_df      - one row per case-level attachment
#   decision_attachments_df  - one row per decision-level attachment
#
# All "code"/"label" pairs are split into two columns, all JSON-string list
# fields (press releases, timeline events, etc.) become their own long tables,
# and empty values are normalized to NA throughout.

library(tidyverse) ; library(jsonlite)

# -----------------------------------------------------------------------------
# 0. Load raw JSON, preserving true nested structure
# -----------------------------------------------------------------------------

raw <- fromJSON("DMA/case-data-DMA.json", simplifyVector = FALSE)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Pull the first element of a length-1 list field, or NA if empty/missing.
# This undoes the "everything is wrapped in an array" pattern.
scalar <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  val <- x[[1]]
  if (is.null(val) || length(val) == 0 || identical(val, "")) return(NA_character_)
  val
}

# Some fields are length-1 lists containing a JSON STRING like
# '{"code":"...","label":"..."}'. Parse that inner string and return
# a named list of code/label (or NA/NA if empty/missing).
code_label <- function(x) {
  s <- scalar(x)
  if (is.na(s)) return(list(code = NA_character_, label = NA_character_))
  parsed <- tryCatch(fromJSON(s), error = function(e) NULL)
  list(
    code  = ifelse(is.null(parsed$code),  NA_character_, parsed$code),
    label = ifelse(is.null(parsed$label), NA_character_, parsed$label)
  )
}

# Some fields are length-1 lists containing a JSON string like
# '{"items":[{...}, {...}]}'. Parse and return a tibble of the items
# (0 rows if empty/missing/malformed).
items_table <- function(x) {
  s <- scalar(x)
  if (is.na(s)) return(tibble())
  parsed <- tryCatch(fromJSON(s), error = function(e) NULL)
  items <- parsed$items
  if (is.null(items) || length(items) == 0) return(tibble())
  # items may be a data.frame (simplified) or a list of empty objects ({})
  items_df <- as_tibble(items)
  if (ncol(items_df) == 0 || nrow(items_df) == 0) return(tibble())
  # drop rows that are entirely empty (i.e. placeholder {} entries)
  items_df %>% filter(if_any(everything(), ~ !is.na(.) & . != ""))
}

# Turn a plain list of scalar strings (e.g. caseCourtCases) into a
# comma-collapsed string, or NA if empty.
collapse_list <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  vals <- map_chr(x, ~ scalar(list(.x)))
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = "; ")
}

# -----------------------------------------------------------------------------
# 1. cases_df — one row per case, flat scalar fields only
# -----------------------------------------------------------------------------

cases_df <- map_dfr(raw, function(case) {
  md <- case$metadata

  origin      <- code_label(md$caseOrigin)
  legal_basis <- code_label(md$caseLegalBasis)
  cps         <- code_label(md$caseCorePlatformServices)
  obligation  <- code_label(md$caseConcernedObligations)

  tibble(
    case_number             = scalar(md$caseNumber),
    case_title               = scalar(md$caseTitle),
    case_type                 = scalar(md$caseType),
    case_dg                    = scalar(md$caseDg),
    case_instrument              = scalar(md$caseInstrument),
    case_initiation_date            = scalar(md$caseInitiationDate),
    case_last_decision_date           = scalar(md$caseLastDecisionDate),
    origin_code                        = origin$code,
    origin_label                        = origin$label,
    legal_basis_code                     = legal_basis$code,
    legal_basis_label                     = legal_basis$label,
    core_platform_service_code             = cps$code,
    core_platform_service_label             = cps$label,
    concerned_obligation_code                = obligation$code,
    concerned_obligation_label                = obligation$label,
    companies                                  = collapse_list(md$caseCompanies),
    sectors                                     = collapse_list(md$caseSectors),
    acquisitions                                 = collapse_list(md$caseAcquisitions),
    designations                                  = collapse_list(md$caseDesignations),
    court_cases                                    = collapse_list(md$caseCourtCases),
    n_case_attachments                              = length(case$caseAttachments %||% list()),
    n_decisions                                      = length(case$decisions %||% list())
  )
}) %>%
  mutate(
    case_initiation_date  = as.Date(case_initiation_date),
    case_last_decision_date = as.Date(case_last_decision_date)
  )

# -----------------------------------------------------------------------------
# 2. decisions_df — one row per decision, linked back to case_number
# -----------------------------------------------------------------------------

decisions_df <- map_dfr(raw, function(case) {
  case_number <- scalar(case$metadata$caseNumber)
  decs <- case$decisions %||% list()

  map_dfr(decs, function(dec) {
    md <- dec$metadata
    dtype <- code_label(md$decisionTypes)

    tibble(
      case_number                 = case_number,
      decision_number               = scalar(md$decisionNumber),
      decision_reference             = scalar(md$metadataReference),
      decision_type_code               = dtype$code,
      decision_type_label               = dtype$label,
      decision_adoption_date              = scalar(md$decisionAdoptionDate),
      n_attachments                         = length(dec$decisionAttachments %||% list())
    )
  })
}) %>%
  mutate(decision_adoption_date = as.Date(decision_adoption_date))

# -----------------------------------------------------------------------------
# 3. decision_press_releases_df — long table, one row per press release
#    (pulled out of the nested JSON-string field on each decision)
# -----------------------------------------------------------------------------

decision_press_releases_df <- map_dfr(raw, function(case) {
  case_number <- scalar(case$metadata$caseNumber)
  decs <- case$decisions %||% list()

  map_dfr(decs, function(dec) {
    md <- dec$metadata
    pr <- items_table(md$decisionPressReleases)
    if (nrow(pr) == 0) return(tibble())
    pr %>% mutate(
      case_number     = case_number,
      decision_number = scalar(md$decisionNumber),
      .before = 1
    )
  })
})

# -----------------------------------------------------------------------------
# 4. case_timeline_events_df — long table, one row per timeline event
# -----------------------------------------------------------------------------

case_timeline_events_df <- map_dfr(raw, function(case) {
  case_number <- scalar(case$metadata$caseNumber)
  ev <- items_table(case$metadata$caseTimelineEvents)
  if (nrow(ev) == 0) return(tibble())
  ev %>% mutate(case_number = case_number, .before = 1)
})

# -----------------------------------------------------------------------------
# 5. case_attachments_df — one row per case-level attachment
# -----------------------------------------------------------------------------

case_attachments_df <- map_dfr(raw, function(case) {
  case_number <- scalar(case$metadata$caseNumber)
  atts <- case$caseAttachments %||% list()

  map_dfr(atts, function(att) {
    md <- att$metadata
    tibble(
      case_number                   = case_number,
      attachment_reference            = scalar(md$metadataReference),
      attachment_category               = scalar(md$attachmentCategory),
      attachment_link                     = scalar(md$attachmentLink),
      attachment_language                   = scalar(md$attachmentLanguage),
      attachment_sent_date                    = scalar(md$attachmentSentDate),
      attachment_publication_date               = scalar(md$attachmentPublicationBusinessDate)
    )
  })
}) %>%
  { if (nrow(.) > 0) mutate(., across(ends_with("_date"), as.Date)) else . }

# -----------------------------------------------------------------------------
# 6. decision_attachments_df — one row per decision-level attachment
# -----------------------------------------------------------------------------

decision_attachments_df <- map_dfr(raw, function(case) {
  case_number <- scalar(case$metadata$caseNumber)
  decs <- case$decisions %||% list()

  map_dfr(decs, function(dec) {
    decision_number <- scalar(dec$metadata$decisionNumber)
    atts <- dec$decisionAttachments %||% list()

    map_dfr(atts, function(att) {
      md <- att$metadata
      tibble(
        case_number                     = case_number,
        decision_number                   = decision_number,
        attachment_reference                = scalar(md$metadataReference),
        attachment_category                   = scalar(md$attachmentCategory),
        attachment_link                         = scalar(md$attachmentLink),
        attachment_language                       = scalar(md$attachmentLanguage),
        attachment_sent_date                        = scalar(md$attachmentSentDate),
        attachment_publication_date                   = scalar(md$attachmentPublicationBusinessDate),
        attachment_id_sequence                          = scalar(md$attachmentIdSequence)
      )
    })
  })
}) %>%
  { if (nrow(.) > 0) mutate(., across(ends_with("_date"), as.Date)) else . }

# -----------------------------------------------------------------------------
# 7. Sanity checks
# -----------------------------------------------------------------------------

cat("cases_df:                  ", nrow(cases_df), "rows\n")
cat("decisions_df:               ", nrow(decisions_df), "rows\n")
cat("decision_press_releases_df:  ", nrow(decision_press_releases_df), "rows\n")
cat("case_timeline_events_df:      ", nrow(case_timeline_events_df), "rows\n")
cat("case_attachments_df:           ", nrow(case_attachments_df), "rows\n")
cat("decision_attachments_df:        ", nrow(decision_attachments_df), "rows\n")

# All decisions should link to a real case
stopifnot(all(decisions_df$case_number %in% cases_df$case_number))

# -----------------------------------------------------------------------------
# 8. Save tidy tables
# -----------------------------------------------------------------------------

write_csv(cases_df,                  "cases_clean.csv")
write_csv(decisions_df,               "decisions_clean.csv")
write_csv(decision_press_releases_df,  "decision_press_releases_clean.csv")
write_csv(case_timeline_events_df,      "case_timeline_events_clean.csv")
write_csv(case_attachments_df,           "case_attachments_clean.csv")
write_csv(decision_attachments_df,        "decision_attachments_clean.csv")
