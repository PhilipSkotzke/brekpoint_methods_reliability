# Runs all 7 breakpoint methods on one session's preprocessed data,
# looping over all three NIRS channels (smo2_dom, smo2_ndom, smo2_avg).
# Returns list(results, data), each tagged with session_id and channel.
run_session_analysis <- function(data, session_id) {
  channels <- c("smo2_dom", "smo2_ndom", "smo2_avg")

  per_channel <- lapply(channels, function(ch) {
    # Copy channel into a fixed column so substitute() inside get_bp_*
    # captures the symbol ".y" rather than the variable name "ch".
    d <- data
    d[[".y"]] <- d[[ch]]

    bp_raw <- get_bp_raw(d, y = .y)
    bp_avg <- get_bp_avg(d, y = .y)
    bp_slope <- get_bp_slope(d, y = .y)
    bp_ratio <- get_bp_ratio(d, y = .y)
    bp_dmax <- get_bp_dmax(d, y = .y)
    bp_dmax_ratio <- get_bp_dmax_ratio(d, y = .y)
    bp_poly <- get_bp_poly(d, y = .y)

    results <- dplyr::bind_rows(
      bp_raw$results,
      bp_avg$results,
      bp_slope$results,
      bp_ratio$results,
      bp_dmax$results,
      bp_dmax_ratio$results,
      bp_poly$results
    ) |>
      tibble::remove_rownames() |>
      dplyr::mutate(session_id = session_id, channel = ch, .before = 1)

    full_data <- dplyr::bind_rows(
      dplyr::mutate(bp_raw$data, method = "raw"),
      dplyr::mutate(bp_avg$data, method = "mean"),
      dplyr::mutate(bp_slope$data, method = "slope"),
      dplyr::mutate(bp_ratio$data, method = "ratio"),
      dplyr::mutate(bp_dmax$data, method = "dmax"),
      dplyr::mutate(bp_dmax_ratio$data, method = "dmax_ratio"),
      dplyr::mutate(bp_poly$data, method = "poly")
    ) |>
      dplyr::mutate(session_id = session_id, channel = ch, .before = 1)

    list(results = results, data = full_data)
  })

  list(
    results = dplyr::bind_rows(lapply(per_channel, `[[`, "results")),
    data = dplyr::bind_rows(lapply(per_channel, `[[`, "data"))
  )
}

# Writes the combined analysis outputs for all sessions to AnalysisData.
# Called once after tar_combine has aggregated across sessions.
# Returns file paths for use with format = "file".
write_analysis <- function(all_results, all_data) {
  dir.create("Data/AnalysisData", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(all_results, "Data/AnalysisData/breakpoint_data.csv")
  readr::write_csv(all_data, "Data/AnalysisData/full_data.csv")
  c("Data/AnalysisData/breakpoint_data.csv", "Data/AnalysisData/full_data.csv")
}
