# load packages needed
library(segmented)
library(lactater)
library(dplyr)
library(ggplot2)

# Helper functions
# --------------------------------------------------------------

# stage means: get last `tail_sec`samples of each stage (1 Hz -> last tail_sec s)
stage_means <- function(data, y_str, laps_str, power_col, tail_sec = 120) {
  has_hr <- "heart_rate" %in% names(data)
  data |>
    dplyr::filter(!is.na(data[[laps_str]]) & !is.na(data[[power_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(laps_str))) |>
    dplyr::summarize(
      power = mean(.data[[power_col]], na.rm = TRUE),
      hr = if (has_hr) {
        mean(tail(.data[["heart_rate"]], tail_sec), na.rm = TRUE)
      } else {
        0
      },
      y = mean(tail(.data[[y_str]], tail_sec), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(power)
}

# same time -> powre mapping as get_bp_raw (linearised ramp equivalent)
time_to_power <- function(
  bp_t,
  starting_load,
  load_increase,
  step_length = 300
) {
  starting_load + bp_t * load_increase / step_length
}

na_result <- function(methods) {
  data.frame(method = methods, bp = 1L, bp_x = NA_real_, power = NA_real_)
}

# Replace power/bp_x with NA when the estimate falls outside the measured range.
validate_bp_range <- function(results, min_power, max_power) {
  out_of_range <- !is.na(results$power) &
    (results$power < min_power | results$power > max_power)
  results$bp_x[out_of_range] <- NA_real_
  results$power[out_of_range] <- NA_real_
  results
}

# Dense x-grid prediction for drawing a fitted model.
# Joining fitted values at the observed x only is not enough for stage-level
# fits: with ~9 stages a segmented kink almost always falls between two stages,
# so the drawn line cuts the corner and the piecewise fit reads as a curve.
# `extra` forces the knots (psi) into the grid so every kink is drawn exactly.
predict_line <- function(model, x_str, x_obs, extra = NULL, n = 300) {
  grid <- sort(unique(c(
    seq(min(x_obs, na.rm = TRUE), max(x_obs, na.rm = TRUE), length.out = n),
    extra[!is.na(extra)]
  )))
  nd <- data.frame(grid)
  names(nd) <- x_str
  fitted <- tryCatch(
    as.numeric(predict(model, newdata = nd)),
    error = function(e) rep(NA_real_, length(grid))
  )
  data.frame(x = grid, fitted = fitted)
}

# Fit double piecewise regression
fit_segmented <- function(data, x_str, y_str, method) {
  data <- data[!is.na(data[[x_str]]) & !is.na(data[[y_str]]), ]
  my_lm <- lm(reformulate(x_str, response = y_str), data = data)

  x_sorted <- sort(unique(data[[x_str]]))
  n <- length(x_sorted)

  offsets <- list(c(0L, 0L), c(1L, -1L), c(-1L, 1L))
  my_seg <- NULL
  seg_warning <- NA_character_
  for (off in offsets) {
    i1 <- max(1L, min(n - 1L, round(n * 0.33) + off[1]))
    i2 <- max(i1 + 1L, min(n, round(n * 0.67) + off[2]))
    captured_warning <- NULL
    fit <- withCallingHandlers(
      tryCatch(
        segmented::segmented(
          my_lm,
          seg.Z = as.formula(paste0("~", x_str)),
          psi = setNames(list(list(x_sorted[i1], x_sorted[i2])), x_str)
        ),
        error = function(e) NULL
      ),
      warning = function(w) {
        captured_warning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    if (!is.null(fit) && !is.null(fit$psi)) {
      my_seg <- fit
      seg_warning <- if (is.null(captured_warning)) {
        NA_character_
      } else {
        captured_warning
      }
      break
    }
  }
  if (is.null(my_seg)) {
    my_seg <- my_lm
  }

  list(
    model = my_seg,
    results = data.frame(
      method = method,
      bp = c(1, 2),
      bp_x = if (is.null(my_seg$psi)) {
        c(NA_real_, NA_real_)
      } else {
        my_seg$psi[, "Est."]
      },
      warning = seg_warning
    ),
    data = data.frame(
      x = data[[x_str]],
      y = data[[y_str]],
      fitted = fitted(my_seg)
    ),
    fit_line = predict_line(
      my_seg,
      x_str,
      data[[x_str]],
      extra = if (is.null(my_seg$psi)) NULL else my_seg$psi[, "Est."]
    )
  )
}

###################################
# ---------- Method 1 ----------- #
# get BPs for smo2 versus time    #
###################################
get_bp_raw <- function(
  data,
  y = "smo2_dom",
  x = time,
  laps = lap,
  power_col = "power",
  step_length = 300
) {
  x_str <- as.character(substitute(x))
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))

  t0 <- min(data[[x_str]], na.rm = TRUE)
  data[[x_str]] <- as.numeric(difftime(data[[x_str]], t0, units = "secs"))

  duration <- data |>
    dplyr::filter(!is.na(.data[[laps_str]])) |>
    dplyr::summarize(max = max(.data[[x_str]])) |>
    dplyr::pull()

  exercise_data <- data[data[[x_str]] <= duration, ]

  starting_load <- mean(
    exercise_data[[power_col]][exercise_data[[laps_str]] == 1],
    na.rm = TRUE
  )
  load_increase <- mean(
    exercise_data[[power_col]][exercise_data[[laps_str]] == 2],
    na.rm = TRUE
  ) -
    starting_load

  out <- fit_segmented(exercise_data, x_str, y_str, method = "raw")
  out$results$power <- time_to_power(
    out$results$bp_x,
    starting_load,
    load_increase,
    step_length
  )
  stage_pows <- tapply(
    exercise_data[[power_col]],
    exercise_data[[laps_str]],
    mean,
    na.rm = TRUE
  )
  out$results <- validate_bp_range(
    out$results,
    min(stage_pows, na.rm = TRUE),
    max(stage_pows, na.rm = TRUE)
  )
  # Lets plot_bp() relabel the seconds axis in watts, the same linear map the
  # method already uses to report its breakpoints in watts.
  out$ramp <- list(
    sl = starting_load,
    li = load_increase,
    step_length = step_length
  )
  out
}

###################################
# ---------- Method 2 ----------- #
# get BPs for smo2 per stage mean #
###################################
get_bp_avg <- function(
  data,
  y = "smo2_dom",
  laps = lap,
  power_col = "power",
  tail_sec = 120
) {
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))
  # 1. aggregate by stage
  stage_data <- stage_means(data, y_str, laps_str, power_col, tail_sec)

  # 2. fit
  out <- fit_segmented(
    stage_data,
    x_str = power_col,
    y_str = "y",
    method = "mean"
  )

  # 3. x is already power — rename t_sec
  out$results$power <- out$results$bp_x
  out$results <- validate_bp_range(
    out$results,
    min(stage_data$power),
    max(stage_data$power)
  )
  out
}


###################################
# ---------- Method 3 ----------- #
# get BPs for smo2 per stage mean #
###################################
# first, small helper function to get the slope
stage_slope_per_min <- function(
  time_vec,
  smo2_vec,
  trim_start = 60,
  trim_end = 10
) {
  t_sec <- as.numeric(difftime(time_vec, min(time_vec), units = "secs"))
  window <- t_sec >= trim_start & t_sec <= (max(t_sec) - trim_end)
  if (sum(window) < 3) {
    return(NA_real_)
  }
  coef(lm(smo2_vec[window] ~ t_sec[window]))[2] * 60 # %/s → %/min
}

# then the function to get the slope BPs
get_bp_slope <- function(
  data,
  x = time,
  y = "smo2_dom",
  laps = lap,
  power_col = "power"
) {
  x_str <- as.character(substitute(x))
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))
  # 1. aggregate by stage
  stage_data <- data |>
    dplyr::filter(!is.na(.data[[laps_str]]) & !is.na(.data[[power_col]])) |>
    group_by(.data[[laps_str]]) |>
    dplyr::summarize(
      power = mean(.data[[power_col]]),
      y = stage_slope_per_min(.data[[x_str]], .data[[y_str]]),
      .groups = "drop"
    )

  # 2. fit
  out <- fit_segmented(
    stage_data,
    x_str = power_col,
    y_str = "y",
    method = "slope"
  )

  # 3. x is already power — rename t_sec
  out$results$power <- out$results$bp_x
  out$results <- validate_bp_range(
    out$results,
    min(stage_data$power),
    max(stage_data$power)
  )
  out
}

###################################
# ---------- Method 4 ----------- #
# get BPs for power/smo2 ratio    #
###################################
get_bp_ratio <- function(
  data,
  y = "smo2_dom",
  laps = lap,
  power_col = "power",
  tail_sec = 120
) {
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))
  # 1. aggregate by stage, then compute power/SmO2 ratio
  stage_data <- stage_means(data, y_str, laps_str, power_col, tail_sec)
  stage_data$y <- stage_data$power / stage_data$y

  # 2. fit
  out <- fit_segmented(
    stage_data,
    x_str = power_col,
    y_str = "y",
    method = "ratio"
  )

  # 3. x is already power
  out$results$power <- out$results$bp_x
  out$results <- validate_bp_range(
    out$results,
    min(stage_data$power),
    max(stage_data$power)
  )
  out
}

###################################
# ---- Dmax / ModDmax engine ----- #
###################################
# Dmax and ModDmax computed directly instead of through lactater.
#
# Both fit a 3rd degree polynomial to the stages above baseline and look for the
# point where the tangent runs parallel to a chord. A cubic has two such points;
# the breakpoint is the one that is actually furthest from the chord. lactater
# instead takes the larger root and falls back to the smaller one whenever the
# fitted value exceeds 8 -- a blood-lactate constant (8 mmol/L) that fires on
# 100% of SmO2 deoxygenation curves and 25% of power/SmO2 ratio curves, so the
# published algorithm never actually ran on this data.
#
# Chords: Dmax spans the first to the last stage. ModDmax starts at the log-log
# breakpoint (the classical "modified Dmax"), taking its height from the fitted
# curve, and its tangent is searched only from that intensity upwards.
# lactater instead anchors ModDmax at the first stage rising by >= 0.4, another
# blood-lactate constant (0.4 mmol/L) that on these scales is met at the very
# first stage in most sessions, collapsing ModDmax onto Dmax.
#
# Both breakpoints are constrained to the recorded power range, i.e. the stages
# the polynomial was actually fitted to; a tangent outside it is extrapolation
# and returns NA rather than a value.
# Returns list(bp = c(Dmax, ModDmax), fit_line, chords, drops), where the last
# three carry the geometry lactater draws: the cubic, the chord each variant is
# measured against, and the vertical maximal-distance segment at the breakpoint.
# The distance is vertical, not perpendicular, because that is what
# furthest_tangent() maximises.
dmax_family <- function(power, value, loglog = NA_real_, degree = 3) {
  na_out <- list(
    bp = c(Dmax = NA_real_, ModDmax = NA_real_),
    fit_line = NULL,
    chords = NULL,
    drops = NULL
  )

  keep <- !is.na(power) & !is.na(value)
  ord <- order(power[keep])
  p <- power[keep][ord]
  v <- value[keep][ord]
  # the first stage is treated as baseline and excluded, as in lactater
  p <- p[-1]
  v <- v[-1]
  if (length(p) < degree + 1) {
    return(na_out)
  }

  cf <- tryCatch(
    unname(coef(lm(v ~ stats::poly(p, degree, raw = TRUE)))),
    error = function(e) NULL
  )
  if (is.null(cf) || anyNA(cf)) {
    return(na_out)
  }
  curve_at <- function(x) {
    cf[1] + cf[2] * x + cf[3] * x^2 + cf[4] * x^3
  }

  lo <- min(p) # recorded power range covered by the fit
  hi <- max(p)

  # tangent parallel to the chord from (x0, y0) to the last stage; of the
  # candidate points inside [from, hi], keep the one furthest from that chord
  furthest_tangent <- function(x0, y0, from = lo) {
    k <- (v[length(v)] - y0) / (p[length(p)] - x0)
    rts <- Re(polyroot(c(cf[2] - k, 2 * cf[3], 3 * cf[4])))
    rts <- rts[rts >= from & rts <= hi]
    if (length(rts) == 0) {
      return(NA_real_)
    }
    rts[which.max(abs(curve_at(rts) - (y0 + k * (rts - x0))))]
  }

  dmax <- furthest_tangent(p[1], v[1])
  moddmax <- NA_real_
  if (!is.na(loglog) && loglog >= lo && loglog < hi) {
    moddmax <- furthest_tangent(loglog, curve_at(loglog), from = loglog)
  }

  # ---- geometry for plotting ----
  grid <- seq(lo, hi, length.out = 300)
  fit_line <- data.frame(x = grid, fitted = curve_at(grid))

  # Chords run to the last stage. Dmax starts at the first fitted stage;
  # ModDmax starts at the log-log intensity, taking its height from the curve.
  x_end <- p[length(p)]
  y_end <- v[length(v)]
  anchors <- list(
    Dmax = c(p[1], v[1]),
    ModDmax = if (is.na(moddmax)) NULL else c(loglog, curve_at(loglog))
  )

  chords <- NULL
  drops <- NULL
  for (nm in names(anchors)) {
    a <- anchors[[nm]]
    bp_x <- if (nm == "Dmax") dmax else moddmax
    if (is.null(a) || is.na(bp_x)) {
      next
    }
    k <- (y_end - a[2]) / (x_end - a[1])
    chords <- rbind(
      chords,
      data.frame(x = a[1], y = a[2], xend = x_end, yend = y_end, variant = nm)
    )
    drops <- rbind(
      drops,
      data.frame(
        x = bp_x,
        y = curve_at(bp_x),
        xend = bp_x,
        yend = a[2] + k * (bp_x - a[1]),
        variant = nm
      )
    )
  }

  list(
    bp = c(Dmax = dmax, ModDmax = moddmax),
    fit_line = fit_line,
    chords = chords,
    drops = drops
  )
}

# Log-log is still lactater's; only the Dmax family needed replacing.
loglog_intensity <- function(
  power,
  value,
  hr = NULL,
  sport = "cycling",
  fit = "3rd degree polynomial"
) {
  # must be a tibble: lactater's prepare_fit() errors on a plain data.frame
  d <- dplyr::tibble(
    power = power,
    value = value,
    hr = if (is.null(hr)) 0 else hr
  )
  res <- tryCatch(
    lactater::lactate_threshold(
      .data = d,
      intensity_column = "power",
      lactate_column = "value",
      heart_rate_column = "hr",
      method = "Log-log",
      fit = fit,
      include_baseline = FALSE,
      sport = sport,
      plot = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(NA_real_)
  }
  out <- as.numeric(res$intensity[as.character(res$method) == "Log-log"])
  if (length(out) == 0) NA_real_ else out[1]
}

###################################
# ---------- Method 5 ----------- #
# log-log, Dmax & ModDmax         #
###################################
get_bp_dmax <- function(
  data,
  y = "smo2_dom",
  laps = lap,
  power_col = "power",
  tail_sec = 120,
  sport = "cycling",
  fit = "3rd degree polynomial"
) {
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))

  # get stage data
  sd <- stage_means(data, y_str, laps_str, power_col, tail_sec)
  # transform Smo2 to deoxygenation to work for Dmax/ModDmax
  sd$deox <- 100 - sd$y

  loglog <- loglog_intensity(sd$power, sd$deox, sd$hr, sport = sport, fit = fit)
  dm <- dmax_family(sd$power, sd$deox, loglog = loglog)

  intensity <- c(loglog, dm$bp[["Dmax"]], dm$bp[["ModDmax"]])
  results <- data.frame(
    method = c("Log-log", "Dmax", "ModDmax"),
    bp = c(1L, 2L, 2L), # Log-log = bp1, Dmax/ModDmax = bp2
    bp_x = intensity,
    power = intensity
  )
  results <- validate_bp_range(results, min(sd$power), max(sd$power))
  list(
    model = NULL,
    results = results,
    data = data.frame(x = sd$power, y = sd$deox, fitted = NA_real_),
    fit_line = dm$fit_line,
    chords = dm$chords,
    drops = dm$drops
  )
}

###################################
# ---------- Method 6 ----------- #
# log-log, Dmax & ModDmax on      #
# power/SmO2 ratio                #
###################################
get_bp_dmax_ratio <- function(
  data,
  y = "smo2_dom",
  laps = lap,
  power_col = "power",
  tail_sec = 120,
  sport = "cycling",
  fit = "3rd degree polynomial"
) {
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))

  # get stage data and compute power/SmO2 ratio
  sd <- stage_means(data, y_str, laps_str, power_col, tail_sec)
  sd$ratio <- sd$power / sd$y

  loglog <- loglog_intensity(
    sd$power,
    sd$ratio,
    sd$hr,
    sport = sport,
    fit = fit
  )
  dm <- dmax_family(sd$power, sd$ratio, loglog = loglog)

  intensity <- c(loglog, dm$bp[["Dmax"]], dm$bp[["ModDmax"]])
  results <- data.frame(
    method = c("Log-log.ratio", "Dmax.ratio", "ModDmax.ratio"),
    bp = c(1L, 2L, 2L),
    bp_x = intensity,
    power = intensity
  )
  results <- validate_bp_range(results, min(sd$power), max(sd$power))
  list(
    model = NULL,
    results = results,
    data = data.frame(x = sd$power, y = sd$ratio, fitted = NA_real_),
    fit_line = dm$fit_line,
    chords = dm$chords,
    drops = dm$drops
  )
}

###################################
# ---------- Method 7 ----------- #
# Polynomial + 2nd derivative     #
###################################
# degree = 4: up to two inflections (BP1, BP2). degree = 3: one.
get_bp_poly <- function(
  data,
  y = "smo2_dom",
  laps = lap,
  power_col = "power",
  tail_sec = 120,
  degree = 4
) {
  y_str <- as.character(substitute(y))
  laps_str <- as.character(substitute(laps))

  sd <- stage_means(data, y_str, laps_str, power_col, tail_sec)

  fit <- tryCatch(
    stats::lm(
      reformulate(
        sprintf("stats::poly(power, %d, raw = TRUE)", degree),
        response = "y"
      ),
      data = sd
    ),
    error = function(e) NULL
  )

  # Coefficients of the 2nd derivative, in increasing powers of x. Its roots
  # are the breakpoints, so it is also worth drawing (see deriv_line below).
  d2 <- NULL
  if (!is.null(fit)) {
    d2 <- tryCatch(
      {
        cf <- unname(coef(fit))
        vapply(2:degree, function(k) k * (k - 1) * cf[k + 1], numeric(1))
      },
      error = function(e) NULL
    )
    if (!is.null(d2) && anyNA(d2)) {
      d2 <- NULL
    }
  }

  bps <- NA_real_
  deriv_line <- NULL
  if (!is.null(d2)) {
    rng <- range(sd$power)
    bps <- tryCatch(
      {
        rts <- polyroot(d2)
        rts <- Re(rts[abs(Im(rts)) < 1e-6])
        sort(rts[rts >= rng[1] & rts <= rng[2]])
      },
      error = function(e) NA_real_
    )
    if (length(bps) == 0) bps <- NA_real_

    grid <- seq(rng[1], rng[2], length.out = 300)
    deriv_line <- data.frame(
      x = grid,
      d2 = vapply(
        grid,
        function(xx) sum(d2 * xx^(seq_along(d2) - 1)),
        numeric(1)
      )
    )
  }

  results <- data.frame(
    method = "poly",
    bp = 1:2,
    bp_x = c(bps[1], if (length(bps) >= 2) bps[2] else NA_real_),
    power = c(bps[1], if (length(bps) >= 2) bps[2] else NA_real_)
  )
  results <- validate_bp_range(results, min(sd$power), max(sd$power))

  list(
    model = fit,
    results = results,
    data = data.frame(
      x = sd$power,
      y = sd$y,
      fitted = if (is.null(fit)) NA_real_ else as.numeric(predict(fit))
    ),
    fit_line = if (is.null(fit)) {
      NULL
    } else {
      predict_line(fit, "power", sd$power)
    },
    deriv_line = deriv_line
  )
}

###################################
# ---------- Reference ---------- #
# breakpoints identified by eye   #
# in the manual rating app        #
###################################
# Same list(results, data) shape as the get_bp_* functions, so manual ratings
# can be plotted like any other method. bp_x is seconds from recording start
# (the axis get_bp_raw() fits on); power is watts, as for every other method.
get_bp_visual <- function(
  data,
  session_id,
  channel,
  rater,
  y = "smo2_dom",
  laps = lap,
  power_col = "power",
  ratings_dir = "Data/ManualRatings",
  step_length = 300
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

  ratings_file <- file.path(ratings_dir, paste0(rater, "_ratings.csv"))
  if (file.exists(ratings_file)) {
    r <- utils::read.csv(ratings_file)
    r <- r[
      r$session_id == session_id &
        r$channel == channel &
        r$method == "visual" &
        !is.na(r$identifiable) &
        r$identifiable,
    ]
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

  # same linear ramp as get_bp_raw(), so a caller can relabel the time axis
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
    ramp = list(sl = sl, li = li, step_length = step_length)
  )
}

###################################
# convenience wrapper             #
# runs all 7 methods for one      #
# session and returns bound       #
# results ready for a targets     #
# pipeline, e.g.:                 #
#   dat |>                        #
#     group_by(session_id) |>     #
#     group_modify(\(d, g)        #
#       get_all_bps(d))           #
###################################
get_all_bps <- function(
  data,
  y_col = "smo2_dom",
  x_col = "time",
  laps_col = "lap",
  power_col = "power",
  tail_sec = 120,
  step_length = 300
) {
  d <- data
  d[[".y"]] <- d[[y_col]]
  d[[".x"]] <- d[[x_col]]
  d[[".lap"]] <- d[[laps_col]]

  dplyr::bind_rows(
    get_bp_raw(
      d,
      y = .y,
      x = .x,
      laps = .lap,
      power_col = power_col,
      step_length = step_length
    )$results,
    get_bp_avg(
      d,
      y = .y,
      laps = .lap,
      power_col = power_col,
      tail_sec = tail_sec
    )$results,
    get_bp_slope(d, y = .y, x = .x, laps = .lap, power_col = power_col)$results,
    get_bp_ratio(
      d,
      y = .y,
      laps = .lap,
      power_col = power_col,
      tail_sec = tail_sec
    )$results,
    get_bp_dmax(
      d,
      y = .y,
      laps = .lap,
      power_col = power_col,
      tail_sec = tail_sec
    )$results,
    get_bp_dmax_ratio(
      d,
      y = .y,
      laps = .lap,
      power_col = power_col,
      tail_sec = tail_sec
    )$results,
    get_bp_poly(
      d,
      y = .y,
      laps = .lap,
      power_col = power_col,
      tail_sec = tail_sec
    )$results
  ) |>
    tibble::remove_rownames()
}

###########################
# plot breakpoint methods #
#    helper function      #
###########################

plot_bp <- function(bp) {
  method <- bp$results$method[1]
  x_label <- c(
    raw = "Time (s)",
    visual = "Time (s)",
    mean = "Power (W)",
    slope = "Power (W)",
    ratio = "Power (W)",
    poly = "Power (W)",
    "Log-log" = "Power (W)",
    Dmax = "Power (W)",
    ModDmax = "Power (W)",
    "Log-log.ratio" = "Power (W)",
    "Dmax.ratio" = "Power (W)",
    "ModDmax.ratio" = "Power (W)"
  )[method]
  y_label <- c(
    raw = "SmO2 (%)",
    visual = "SmO2 (%)",
    mean = "SmO2 (%)",
    slope = "SmO2 slope (%/min)",
    ratio = "Power/SmO2 (W/%)",
    poly = "SmO2 (%)",
    "Log-log" = "Deoxygenation (%)",
    Dmax = "Deoxygenation (%)",
    ModDmax = "Deoxygenation (%)",
    "Log-log.ratio" = "Power/SmO2 (W/%)",
    "Dmax.ratio" = "Power/SmO2 (W/%)",
    "ModDmax.ratio" = "Power/SmO2 (W/%)"
  )[method]
  title_label <- paste(unique(bp$results$method), collapse = " / ")

  # Time-domain panels are relabelled onto the power axis so that every panel
  # shares one x scale and a breakpoint at a given wattage sits at the same
  # position throughout. The ramp is linear in time, so this is an affine
  # relabel: the curve shape is unchanged.
  if (!is.null(bp$ramp)) {
    to_power <- function(t) {
      time_to_power(t, bp$ramp$sl, bp$ramp$li, bp$ramp$step_length)
    }
    bp$data$x <- to_power(bp$data$x)
    if (!is.null(bp$fit_line)) {
      bp$fit_line$x <- to_power(bp$fit_line$x)
    }
    x_label <- "Power (W)"
  }

  # results$power is the wattage for every method, so it is the axis-consistent
  # position for the breakpoint markers.
  valid_bps <- bp$results[!is.na(bp$results$power), ]

  # Dense recordings get a connecting trace; stage-level fits are points only.
  is_timeseries <- method %in% c("raw", "visual")
  fixed_scale <- method %in%
    c("raw", "visual", "mean", "poly", "Log-log", "Dmax", "ModDmax")
  # purple, not red: the cubic fit is already red
  dmax_cols <- c(Dmax = "#1565C0", ModDmax = "#6A1B9A")
  deriv_col <- "#2E7D32"

  p <- ggplot2::ggplot(bp$data, ggplot2::aes(x = x, y = y))

  # Time series: the trace goes down first so the fit reads on top of it.
  if (is_timeseries) {
    p <- p + ggplot2::geom_line(colour = "grey60")
  }

  # Fitted model. Prefer the dense fit_line so segmented kinks land exactly on
  # the knots; fall back to the fitted values at the observed x when absent.
  if (!is.null(bp$fit_line) && !all(is.na(bp$fit_line$fitted))) {
    p <- p +
      ggplot2::geom_line(
        data = bp$fit_line,
        mapping = ggplot2::aes(x = x, y = fitted),
        inherit.aes = FALSE,
        colour = "red",
        linewidth = 1.2
      )
  } else if (!all(is.na(bp$data$fitted))) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = fitted),
        colour = "red",
        linewidth = 1.2
      )
  }

  # Dmax family: the chord each variant is measured against, plus the vertical
  # maximal-distance segment from the curve down to that chord.
  if (!is.null(bp$chords)) {
    p <- p +
      ggplot2::geom_segment(
        data = bp$chords,
        mapping = ggplot2::aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend,
          colour = variant
        ),
        inherit.aes = FALSE,
        linetype = "dashed",
        linewidth = 0.7
      )
  }
  if (!is.null(bp$drops)) {
    p <- p +
      ggplot2::geom_segment(
        data = bp$drops,
        mapping = ggplot2::aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend,
          colour = variant
        ),
        inherit.aes = FALSE,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = bp$drops,
        mapping = ggplot2::aes(x = x, y = y, colour = variant),
        inherit.aes = FALSE,
        size = 2.5
      )
  }

  # Derivative on a secondary axis, rescaled so its full range covers exactly
  # the same vertical extent as the primary axis; the secondary axis carries
  # the inverse transform so its labels read in the derivative's own units.
  sec <- NULL
  if (!is.null(bp$deriv_line) && !all(is.na(bp$deriv_line$d2))) {
    y_rng <- if (fixed_scale) c(0, 100) else range(bp$data$y, na.rm = TRUE)
    d_rng <- range(bp$deriv_line$d2, na.rm = TRUE)
    span_y <- diff(y_rng)
    span_d <- if (diff(d_rng) == 0) 1 else diff(d_rng)
    to_primary <- function(d) (d - d_rng[1]) / span_d * span_y + y_rng[1]
    to_deriv <- function(yy) (yy - y_rng[1]) / span_y * span_d + d_rng[1]

    dl <- bp$deriv_line
    dl$y2 <- to_primary(dl$d2)
    # Zero of the derivative: the breakpoints are its crossings. Only drawn
    # when zero falls inside the panel -- if it does not, the derivative never
    # crosses it and the method returns no breakpoint.
    if (to_primary(0) >= y_rng[1] && to_primary(0) <= y_rng[2]) {
      p <- p +
        ggplot2::geom_hline(
          yintercept = to_primary(0),
          colour = deriv_col,
          linetype = "dotted",
          linewidth = 0.5
        )
    }
    p <- p +
      ggplot2::geom_line(
        data = dl,
        mapping = ggplot2::aes(x = x, y = y2),
        inherit.aes = FALSE,
        colour = deriv_col,
        linewidth = 1
      )
    sec <- ggplot2::sec_axis(~ to_deriv(.), name = "2nd derivative")
  }

  # Observed stages last, so the points sit on top of the fit.
  if (!is_timeseries) {
    p <- p + ggplot2::geom_point()
  }

  p <- p +
    ggplot2::geom_vline(xintercept = valid_bps$power, linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = valid_bps$power,
      y = Inf,
      vjust = 1.5,
      label = paste0(round(valid_bps$power), " W")
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = title_label, x = x_label, y = y_label)

  if (fixed_scale) {
    p <- p +
      ggplot2::scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        sec.axis = if (is.null(sec)) ggplot2::waiver() else sec
      )
  } else if (!is.null(sec)) {
    p <- p + ggplot2::scale_y_continuous(sec.axis = sec)
  }

  if (!is.null(sec)) {
    p <- p +
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(colour = deriv_col),
        axis.text.y.right = ggplot2::element_text(colour = deriv_col)
      )
  }

  if (!is.null(bp$chords) || !is.null(bp$drops)) {
    p <- p +
      ggplot2::scale_colour_manual(values = dmax_cols, name = NULL) +
      ggplot2::theme(legend.position = "bottom")
  }

  p
}
