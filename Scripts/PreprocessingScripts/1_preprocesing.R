preprocess_mnirs <- function(filename, metadata) {
  id_num <- as.integer(sub("ID(\\d+)_(\\d+)\\.csv", "\\1", filename))
  meta <- metadata[metadata$ID == id_num, ]

  # "Starkes Bein": 0 = left dominant, 1 = right dominant
  is_right_dom <- nrow(meta) > 0 && meta[["Starkes Bein"]][[1L]] == 1L

  channels <- if (is_right_dom) {
    c(
      smo2_dom = "3rd SmO2 Sensor 6988 on R. Quad",
      smo2_ndom = "2nd SmO2 Sensor 6990 on L. Quad"
    )
  } else {
    c(
      smo2_dom = "2nd SmO2 Sensor 6990 on L. Quad",
      smo2_ndom = "3rd SmO2 Sensor 6988 on R. Quad"
    )
  }

  dat <- mnirs::read_mnirs(
    file.path("Data/InputData/recordings", filename),
    nirs_channels = channels,
    time_channel = c(time = "timestamp"),
    sample_rate = 0.5,
    keep_all = TRUE,
    verbose = FALSE
  ) |>
    mnirs::resample_mnirs(
      resample_rate = 1,
      method = "linear"
    ) |>
    mnirs::replace_mnirs(
      invalid_values = c(0, 100),
      method = "linear",
      outlier_cutoff = 3,
      span = 60
    ) |>
    mnirs::filter_mnirs(
      method = "butter",
      W = 0.02
    ) |>
    dplyr::mutate(dplyr::across(
      where(is.character),
      ~ suppressWarnings(as.numeric(.x))
    )) |>
    dplyr::mutate(smo2_avg = (.data[["smo2_dom"]] + .data[["smo2_ndom"]]) / 2)

  dat
}

add_power <- function(dat, filename, metadata, stage_duration_sec = 300L) {
  id_num <- as.integer(sub("ID(\\d+)_(\\d+)\\.csv", "\\1", filename))
  session_num <- as.integer(sub("ID(\\d+)_(\\d+)\\.csv", "\\2", filename))

  dat$participant_id <- id_num
  dat$session <- session_num

  meta <- metadata[metadata$ID == id_num, ]
  if (nrow(meta) == 0L) {
    return(dat)
  }

  zeit_col <- paste0("Zeit letzte Stufe_", session_num)
  if (!zeit_col %in% names(meta)) {
    return(dat)
  }

  # Last stage duration: Excel stores it as a time-of-day (HH:MM:SS) with
  # the date 1899-12-31; extract just the time component as seconds
  zeit_val <- meta[[zeit_col]][[1L]]
  last_stage_sec <- as.integer(
    lubridate::hour(zeit_val) *
      3600L +
      lubridate::minute(zeit_val) * 60L +
      as.integer(lubridate::second(zeit_val))
  )

  # Number of completed stages = non-NA lactate values for this session
  laktat_cols <- grep(
    paste0("^[0-9]+,[0-9]+_", session_num, "$"),
    names(metadata),
    value = TRUE
  )
  n_stages <- sum(!is.na(meta[laktat_cols]))
  if (n_stages == 0L) {
    return(dat)
  }

  # Stage powers (watts): Start, Start+Step, Start+2*Step, ...
  stage_powers_w <- meta$Start[[1L]] +
    (seq_len(n_stages) - 1L) * meta$Steigerung[[1L]]

  # Recording starts exactly at test start; stages are contiguous
  t0 <- min(dat$time, na.rm = TRUE)
  stage_starts <- t0 + (seq_len(n_stages) - 1L) * stage_duration_sec
  stage_ends <- c(stage_starts[-1L], stage_starts[n_stages] + last_stage_sec)

  dat$power <- NA_real_
  dat$lap <- NA_integer_
  for (i in seq_len(n_stages)) {
    in_stage <- dat$time >= stage_starts[i] & dat$time < stage_ends[i]
    dat$power[in_stage] <- stage_powers_w[i]
    dat$lap[in_stage] <- i
  }

  dat
}
