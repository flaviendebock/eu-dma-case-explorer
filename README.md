# DMA Enforcement Tracker

Clean and visualize the European Commission's [Digital Markets Act](https://digital-markets-act.ec.europa.eu/) case data, published as a nested JSON export. This repo turns that raw export into tidy CSV tables and a PDF report of charts covering enforcement activity across Apple, Alphabet, Meta, Microsoft, Amazon, ByteDance, and other designated gatekeepers.

## What is in here

```
.
├── case-data-DMA.json          Raw source data (not tracked, see Data below)
├── clean_dma_data.R            Parses the raw JSON into tidy, linked CSV tables
├── visualize_dma_data.R        Standalone script that saves each chart as a PNG
├── dma_report.Rmd              R Markdown report that knits all charts into one PDF
└── README.md
```

## Data

The source file is the DMA case data export from the [European Union data portal](https://data.europa.eu/data/datasets/72358eb1-37aa-40bb-8047-d87154d57ac1). Download `case-data-DMA.json` from that page and place it in the repo root before running anything.

The raw export wraps almost every field in a length one array, and several fields (press releases, timeline events, code and label pairs) are JSON encoded as a string one level down. `clean_dma_data.R` reads the file with `jsonlite::fromJSON(..., simplifyVector = FALSE)` to preserve the true nested structure, then walks it explicitly rather than relying on jsonlite's automatic flattening.

## Setup

You need R with the following packages:

```r
install.packages(c("tidyverse", "jsonlite", "scales", "rmarkdown", "knitr"))
```

Knitting the report to PDF also requires a LaTeX distribution. The easiest route is [tinytex](https://yihui.org/tinytex/):

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Usage

Run the three scripts in order from the repo root.

**1. Clean the raw data**

```r
Rscript clean_dma_data.R
```

Produces six tidy tables, each written as its own CSV:

| File | Contents |
|---|---|
| `cases_clean.csv` | One row per case, with company, platform service, obligation, and origin fields split out |
| `decisions_clean.csv` | One row per decision, linked to `cases_clean.csv` by `case_number` |
| `decision_press_releases_clean.csv` | One row per press release tied to a decision |
| `case_timeline_events_clean.csv` | One row per publication timeline event |
| `case_attachments_clean.csv` | One row per case level attachment |
| `decision_attachments_clean.csv` | One row per decision level attachment |

**2. Generate the charts (optional, as standalone PNGs)**

```r
Rscript visualize_dma_data.R
```

**3. Knit the full report**

```r
Rscript -e 'rmarkdown::render("dma_report.Rmd")'
```

Produces `dma_report.pdf`, a report covering:

- Decisions per year, split between procedural and substantive outcomes
- Cases still open with zero decisions, ranked by days open
- Which DMA obligations show up most often
- Case volume by company and by core platform service
- The overall mix of decision types

## Notes on the data

Company names in the raw export are inconsistent, for example different punctuation for the same Amazon entity, or the two Apple entities listed in a different order across cases. Both `visualize_dma_data.R` and `dma_report.Rmd` normalize these into one canonical name per company before counting.

About a third of cases have no obligation article attached yet, since they are still at the designation stage rather than tied to a specific compliance investigation.

## License

Add a license of your choice here. The underlying case data belongs to the European Commission; check the [source dataset page](https://data.europa.eu/data/datasets/72358eb1-37aa-40bb-8047-d87154d57ac1) for its terms of reuse.
