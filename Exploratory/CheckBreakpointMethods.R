# Visual check of the breakpoint methods: runs every get_bp_* function from
# 0_helper_functions.R on a single session/channel and shows them side by side,
# together with the visually identified breakpoints from the manual rating app.
# This is the "visual checks" block sketched at the bottom of 2_analysis.R.

library(dplyr)
library(ggplot2)
library(patchwork)

source(file.path("Scripts", "PreprocessingScripts", "0_helper_functions.R"))

# --- pick a session, channel and rater -------------------------------------
ID <- "ID07"
test <- "01"
channel <- "smo2_dom" # smo2_dom, smo2_ndom or smo2_avg
rater <- "Rater_A" # file in Data/ManualRatings/<rater>_ratings.csv

session_id <- paste0(ID, "_", test)

filepath <- file.path(
  "Data",
  "IntermediateData",
  paste0(session_id, "_preprocessed.csv")
)

dat <- read.csv(filepath)

# Copy the channel into a fixed column so substitute() inside get_bp_*
# captures the symbol ".y" rather than the variable name "channel".
dat[[".y"]] <- dat[[channel]]

# --- visually identified breakpoints ---------------------------------------
# Returns the same list(results, data) shape as the get_bp_* functions, so the
# manual ratings can be plotted with plot_bp() like any other method.
# bp_x is seconds from recording start, i.e. the same axis as get_bp_raw().
get_bp_visual <- function(
  data,
  session_id,
  channel,
  rater,
  y = "smo2_dom",
  laps = lap,
  power_col = "power"
) {
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))

  # exercise portion only, matching get_bp_raw()
  duration <- max(data$time[!is.na(data[[laps_str]])], na.rm = TRUE)
  exercise_data <- data[data$time <= duration, ]

  # always two rows, so the panel still draws when a rating is missing
  results <- data.frame(
    method = "visual",
    bp = 1:2,
    bp_x = NA_real_,
    power = NA_real_
  )

  ratings_file <- file.path(
    "Data",
    "ManualRatings",
    paste0(rater, "_ratings.csv")
  )
  if (file.exists(ratings_file)) {
    r <- read.csv(ratings_file) |>
      dplyr::filter(
        .data$session_id == !!session_id,
        .data$channel == !!channel,
        .data$method == "visual",
        !is.na(.data$identifiable) & .data$identifiable
      )
    for (n in 1:2) {
      row <- r[r$bp == n, ]
      if (nrow(row) == 1) {
        results$bp_x[n] <- row$bp_x
        results$power[n] <- row$power
      }
    }
  } else {
    warning("No ratings file found: ", ratings_file)
  }

  # same linear ramp as get_bp_raw(), so plot_bp() puts this panel on the
  # shared power axis too
  sl <- mean(
    exercise_data[[power_col]][exercise_data[[laps_str]] == 1],
    na.rm = TRUE
  )
  li <- mean(
    exercise_data[[power_col]][exercise_data[[laps_str]] == 2],
    na.rm = TRUE
  ) -
    sl

  list(
    model = NULL,
    results = results,
    data = data.frame(
      x = exercise_data$time,
      y = exercise_data[[y_str]],
      fitted = NA_real_
    ),
    ramp = list(sl = sl, li = li, step_length = 300)
  )
}

# --- run every method ------------------------------------------------------
# Listed in panel order. With ncol = 2 each row is a pair sharing one signal:
#   row 1  SmO2 vs time          visual (rated by eye)  | raw (segmented)
#   row 2  stage-mean SmO2       mean (segmented)       | poly (4th order)
#   row 3  power/SmO2 ratio      ratio (segmented)      | dmax_ratio
#   row 4  remaining transforms  dmax (deoxygenation)   | slope (%/min)
bps <- list(
  visual = get_bp_visual(dat, session_id, channel, rater, y = .y),
  raw = get_bp_raw(dat, y = .y),
  mean = get_bp_avg(dat, y = .y),
  poly = get_bp_poly(dat, y = .y),
  ratio = get_bp_ratio(dat, y = .y),
  dmax_ratio = get_bp_dmax_ratio(dat, y = .y),
  dmax = get_bp_dmax(dat, y = .y),
  slope = get_bp_slope(dat, y = .y)
)

# --- shared x axis ----------------------------------------------------------
# Every panel is drawn on watts: the two time-domain panels carry a $ramp,
# which plot_bp() uses to relabel their seconds axis via time_to_power().
# One common range across all eight puts a given wattage at the same
# horizontal position everywhere, so breakpoints compare by eye down the grid.
x_range_w <- function(bp) {
  x <- bp$data$x
  if (!is.null(bp$ramp)) {
    x <- time_to_power(x, bp$ramp$sl, bp$ramp$li, bp$ramp$step_length)
  }
  range(x, na.rm = TRUE)
}
x_lim <- range(vapply(bps, x_range_w, numeric(2)))

# --- panel of all methods --------------------------------------------------
panel <- (patchwork::wrap_plots(lapply(bps, plot_bp), ncol = 2) &
  ggplot2::coord_cartesian(xlim = x_lim)) +
  patchwork::plot_annotation(
    title = paste0(session_id, " — ", channel, " (visual: ", rater, ")")
  )

panel

# --- breakpoints in one table ----------------------------------------------
results <- dplyr::bind_rows(lapply(bps, `[[`, "results")) |>
  tibble::remove_rownames()

print(results)
