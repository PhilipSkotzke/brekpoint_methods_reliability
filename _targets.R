##  Setup ====================================
suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
})

tar_option_set(
  packages = c(
    "tidyr",
    "dplyr",
    "ggplot2",
    "ggrepel",
    "patchwork",
    "mnirs",
    "readxl",
    "lubridate",
    "readr",
    "segmented",
    "lactater",
    "tibble",
    "systemfonts",
    "ragg",
    "ggtext",
    "ggpattern",
    "tidytext",
    "scales"
  ),
  error = "continue"
)

tar_source(files = c("Scripts/PreprocessingScripts", "Scripts/AnalysisScripts"))

# Session used for the poster's per-method example panel (visuals.R)
VISUALS_SESSION <- "ID07_01"

# Raw recordings are participant data and are not distributed with this repo
# (see README). When they're absent, fall back to the committed
# Data/AnalysisData + Data/IntermediateData files so the report and poster
# figures can still be reproduced from a plain clone.
has_raw_data <- dir.exists("Data/InputData/recordings") &&
  length(list.files("Data/InputData/recordings", pattern = "\\.csv$")) > 0 &&
  file.exists("Data/InputData/summary.xlsx")

manual_ratings_target <- targets::tar_target(
  manual_ratings_file,
  "Data/ManualRatings/Rater_A_ratings.csv",
  format = "file"
)
if (has_raw_data) {
  # One row per recording file; file_id becomes the target name suffix
  recording_files <- data.frame(
    filename = list.files("Data/InputData/recordings", pattern = "\\.csv$"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(file_id = tools::file_path_sans_ext(filename))

  # Per-file targets: signal preprocessing, power/stage labelling, CSV export,
  # and breakpoint analysis
  mapped <- tarchetypes::tar_map(
    values = recording_files,
    names = file_id,
    targets::tar_target(
      nirs_clean,
      preprocess_mnirs(filename, metadata_laktat)
    ),
    targets::tar_target(
      nirs_processed,
      add_power(nirs_clean, filename, metadata_laktat)
    ),
    targets::tar_target(
      nirs_file,
      {
        path <- file.path(
          "Data/IntermediateData",
          paste0(tools::file_path_sans_ext(filename), "_preprocessed.csv")
        )
        dir.create(
          "Data/IntermediateData",
          recursive = TRUE,
          showWarnings = FALSE
        )
        readr::write_csv(nirs_processed, path)
        path
      },
      format = "file"
    ),
    targets::tar_target(bps, run_session_analysis(nirs_processed, file_id)),
    targets::tar_target(bp_results, bps$results),
    targets::tar_target(bp_data, bps$data)
  )

  combined_results <- tarchetypes::tar_combine(
    all_bp_results,
    mapped[["bp_results"]],
    command = dplyr::bind_rows(!!!.x)
  )

  combined_data <- tarchetypes::tar_combine(
    all_bp_data,
    mapped[["bp_data"]],
    command = dplyr::bind_rows(!!!.x)
  )

  data_targets <- list(
    targets::tar_target(
      summary_xlsx,
      "Data/InputData/summary.xlsx",
      format = "file"
    ),
    targets::tar_target(
      metadata_laktat,
      readxl::read_excel(summary_xlsx, sheet = "Laktat")
    ),
    mapped,
    combined_results,
    combined_data,
    targets::tar_target(
      analysis_files,
      write_analysis(all_bp_results, all_bp_data),
      format = "file"
    ),
    manual_ratings_target
  )

  visuals_target <- targets::tar_target(
    visuals,
    {
      force(report)
      force(nirs_file_ID07_01)
      make_poster_figures(VISUALS_SESSION)
    },
    format = "file"
  )
} else {
  message(
    "Data/InputData not found -- skipping raw preprocessing and using the ",
    "already-committed Data/AnalysisData / Data/IntermediateData files. ",
    "Raw NIRS recordings are participant data and are not distributed with ",
    "this repo; contact the authors for access if you need to regenerate ",
    "them from scratch."
  )

  data_targets <- list(
    targets::tar_target(
      analysis_files,
      {
        paths <- c(
          "Data/AnalysisData/breakpoint_data.csv",
          "Data/AnalysisData/full_data.csv"
        )
        missing <- paths[!file.exists(paths)]
        if (length(missing) > 0) {
          stop(
            "Missing precomputed analysis file(s) and no raw data to ",
            "regenerate them: ",
            paste(missing, collapse = ", ")
          )
        }
        paths
      },
      format = "file"
    ),
    targets::tar_target(
      visuals_session_file,
      {
        path <- file.path(
          "Data/IntermediateData",
          paste0(VISUALS_SESSION, "_preprocessed.csv")
        )
        if (!file.exists(path)) {
          stop(
            "Missing precomputed intermediate file and no raw data to ",
            "regenerate it: ",
            path
          )
        }
        path
      },
      format = "file"
    ),
    manual_ratings_target
  )

  visuals_target <- targets::tar_target(
    visuals,
    {
      force(report)
      force(visuals_session_file)
      make_poster_figures(VISUALS_SESSION)
    },
    format = "file"
  )
}

report_target <- targets::tar_target(
  report,
  {
    force(analysis_files)
    force(manual_ratings_file)
    out_file <- "main_analysis.html"
    quarto::quarto_render(
      input = "Scripts/AnalysisScripts/main_analysis.qmd",
      output_file = out_file,
      quiet = FALSE
    )
    from <- file.path("Scripts/AnalysisScripts", out_file)
    to <- file.path("Output", out_file)
    dir.create("Output", recursive = TRUE, showWarnings = FALSE)
    if (file.exists(from)) {
      file.rename(from, to)
    }
    c(to, "Data/AnalysisData/icc_all.rds")
  },
  format = "file"
)

c(
  data_targets,
  list(report_target, visuals_target)
)
