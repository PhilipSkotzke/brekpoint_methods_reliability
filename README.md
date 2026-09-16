# Not all breakpoints are created equally

*Test-retest reliability and bilateral differences of SmO₂ breakpoints during continuous cycling step tests*

Supplementary code and analysis for the poster presented at mNIRS 2026.

A rendered version of the full analysis is available at [`Output/main_analysis.html`](Output/main_analysis.html) — download and open it locally to view results without running the pipeline (GitHub displays it as raw source rather than rendering it in-browser).

In the `Output/` folder you will also find:

- the poster as PDF [`Poster_mNIRS26_Skotzke.pdf`](Output/Poster_mNIRS26_Skotzke.pdf)
- the one-slide presentation [`Summaryslide_mNIRS26_Skotzke.pdf`](Output/Summaryslide_mNIRS26_Skotzke.pdf)
- corrected abstract [`Abstract_mnirs_corrected.pdf`](Output/Abstract_mnirs_corrected.pdf)

## Background

A lot of research interest has focused on identifying breakpoints in NIRS-derived measures. Some studies find no significant difference between methods, others report breakpoints appearing at different workloads. One challenge is that even when the mean difference between methods is low, individual-level agreement is often poor — day-to-day variability of the NIRS breakpoint (or of the reference threshold) may be a major contributor.

While day-to-day variability of ventilatory and lactate thresholds has been studied extensively, little is known about the test-retest reliability of NIRS breakpoints, and most proposed NIRS breakpoint methods have only been evaluated for criterion validity, not reliability.

This project computes multiple breakpoint methods on a pre-existing data set of repeated incremental cycling tests and reports their test-retest reliability, to reveal differences between methods and inform future work on criterion validity and physiological interpretation of NIRS breakpoints.

### Sample

12 participants, 3 repeated incremental cycling sessions each (36 recordings). SmO₂ was recorded on both the dominant and non-dominant leg; a per-participant average channel is also analyzed.

### Breakpoint methods

Two breakpoints (BP1, BP2) are identified per session/channel, each using several methods:

**BP1:** visual identification; double-segmented regression on the raw signal, on stage-mean SmO₂, on the per-stage slope, and on the power/SmO₂ ratio; log-log method on stage-mean SmO₂ and on the power/SmO₂ ratio; 1st inflection of a 4th-degree polynomial fit.

**BP2:** visual identification; double-segmented regression on the raw signal, on stage-mean SmO₂, on the per-stage slope, and on the power/SmO₂ ratio; Dmax and modified-Dmax on stage-mean SmO₂ and on the power/SmO₂ ratio; 2nd inflection of a 4th-degree polynomial fit.

## Repository structure

```
Data/
  InputData/          Raw recordings + protocol metadata (NOT in this repo, see below)
  IntermediateData/    Preprocessed per-session signals (filtered, power-labelled)
  AnalysisData/        Combined breakpoint results, full data, and ICC/reliability table
  ManualRatings/        Visually-identified breakpoints from the rating app
Scripts/
  PreprocessingScripts/ Signal preprocessing, breakpoint-detection methods, analysis helpers
  AnalysisScripts/      main_analysis.qmd (reliability report), visuals.R (poster figures)
  ManualRating/          Shiny app for visual breakpoint rating
Exploratory/            Ad hoc / scratch scripts, not part of the pipeline
Output/                 Rendered report (main_analysis.html) and poster figures
_targets.R              targets pipeline definition
_targets/                targets pipeline cache (not tracked)
run.R                    Entry point
```

## Data availability

Raw NIRS recordings and the protocol summary spreadsheet (`Data/InputData/`) are participant data and are **not distributed** in this repository. Please contact the authors if you need access.

The already-processed data needed to reproduce the report and figures **is** included: `Data/IntermediateData/`, `Data/AnalysisData/`, and `Data/ManualRatings/`. The pipeline (see below) detects whether raw data is present and automatically falls back to these committed files when it isn't, so the report and poster figures can be regenerated from a plain clone without the raw recordings.

## Requirements

- R 4.6.0 (see `renv.lock`)
- [`renv`](https://rstudio.github.io/renv/) for package management
- [Quarto CLI](https://quarto.org/docs/get-started/) installed on your system to render `main_analysis.qmd`

## Reproducing the analysis

1. Clone the repository and open an R session with the repo root as the working directory (e.g. `setwd()`, or open the folder in RStudio/VS Code).
2. Restore the package library:

   ```r
   renv::restore()
   ```

3. Run the pipeline:

   ```r
   source("run.R")
   ```

   or directly:

   ```r
   targets::tar_make()
   ```

This runs the full `targets` pipeline: signal preprocessing → per-session breakpoint detection → combining results across sessions → rendering the reliability report (`Output/main_analysis.html`) → generating the poster figures (`Output/Fig_Panel.png`, `Output/Fig_Rel.png`).

Inspect the pipeline graph with:

```r
targets::tar_visnetwork()
```

## Manual breakpoint rating

Visual breakpoint identification is done via a Shiny app:

```r
shiny::runApp("Scripts/ManualRating/manual_rating_app.R")
```

Ratings are saved to `Data/ManualRatings/<rater>_ratings.csv`.

## License

MIT — see [LICENSE](LICENSE).

## Contact

Philip Skotzke — [p.skotzke@studenti.uniroma4.it](mailto:p.skotzke@studenti.uniroma4.it)
