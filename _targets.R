#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

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
    "tibble"
  ),
  error = "continue"
)

tar_source(files = "Scripts/PreprocessingScripts")

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
  # run the preprocess script
  targets::tar_target(nirs_clean, preprocess_mnirs(filename, metadata_laktat)),
  # run the add power script
  targets::tar_target(
    nirs_processed,
    add_power(nirs_clean, filename, metadata_laktat)
  ),
  # store intermediate data
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
  # run bp methods
  targets::tar_target(bps, run_session_analysis(nirs_processed, file_id)),
  # get the results for combining
  targets::tar_target(bp_results, bps$results),
  # get the data for combining
  targets::tar_target(bp_data, bps$data)
)

# Combine per-session results and data across all recordings
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

list(
  # Track the xlsx file so any change invalidates downstream targets
  targets::tar_target(
    summary_xlsx,
    "Data/InputData/summary.xlsx",
    format = "file"
  ),

  # Protocol metadata (Start, Steigerung, Zeit letzte Stufe per participant/session)
  targets::tar_target(
    metadata_laktat,
    readxl::read_excel(summary_xlsx, sheet = "Laktat")
  ),

  # Per-file preprocessing + breakpoint analysis
  mapped,

  # Combined analysis outputs across all sessions
  combined_results,
  combined_data,

  # Write combined outputs to AnalysisData
  targets::tar_target(
    analysis_files,
    write_analysis(all_bp_results, all_bp_data),
    format = "file"
  ),

  # render Quarto report
  targets::tar_target(
    report,
    {
      force(all_bp_results)
      force(all_bp_data)
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
      to
    },
    format = "file"
  )
)
