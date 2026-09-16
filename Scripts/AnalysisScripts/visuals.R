################################
# In this file all figures for #
#  the mNIRS 2026 poster are   #
#      created and saved       #
################################

# ------
# Set-Up
# ------

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(systemfonts) # finds the OS-installed poster fonts
library(ragg) # the only device that then actually renders them
library(ggtext) # markdown-rendered titles/axis text, for SmO<sub>2</sub>
library(ggpattern)

source(file.path("Scripts", "PreprocessingScripts", "0_helper_functions.R"))

# ----------------------
# University Color Theme
# ----------------------

# Categorical series colours, in the order they should be used.
FI_BLUE <- "#186594" # 1 primary blue
FI_GOLD <- "#E0A32E" # 2 gold
FI_TEAL <- "#1B9E96" # 3 teal
FI_VIOLET <- "#6E57C0" # 4 violet
FI_CORAL <- "#DB5A48" # 5 coral
FI_NAVY <- "#031C48" # 6 dark navy

FI_PALETTE <- c(FI_BLUE, FI_GOLD, FI_TEAL, FI_VIOLET, FI_CORAL, FI_NAVY)

# Single-series plots use FI_BLUE; to flag ONE key bar/point use FI_GOLD and
# leave the rest FI_BLUE.
FI_SEQUENTIAL <- c(FI_NAVY, FI_BLUE, "#6EC1E4") # low -> high

FI_INK <- "#1A1A1A" # all text, axis lines, ticks
FI_GRID <- "#DCE3EC" # subtle gridlines

# Figures sit on white cards over a #F1F4F8 page, so they are saved with a
# transparent background. These are the only acceptable solid fills.
FI_CARD <- "#FFFFFF"
FI_PAGE <- "#F1F4F8"

# Fonts must be installed on the OS; systemfonts/ragg read them from there.
# Without the check they fall back silently and the poster mismatches.
FI_FONT <- "Roboto" # everything
FI_FONT_TITLE <- "Roboto Slab" # optional plot titles only

.fi_fonts <- systemfonts::system_fonts()$family
if (!FI_FONT %in% .fi_fonts) {
  warning("Font '", FI_FONT, "' is not installed -- falling back to 'sans'.")
  FI_FONT <- "sans"
}
if (!FI_FONT_TITLE %in% .fi_fonts) {
  warning("Font '", FI_FONT_TITLE, "' is not installed -- using ", FI_FONT, ".")
  FI_FONT_TITLE <- FI_FONT
}

scale_colour_fi <- function(...) {
  scale_colour_manual(values = FI_PALETTE, ...)
}
scale_fill_fi <- function(...) {
  scale_fill_manual(values = FI_PALETTE, ...)
}
scale_colour_fi_c <- function(...) {
  scale_colour_gradientn(colours = FI_SEQUENTIAL, ...)
}
scale_fill_fi_c <- function(...) {
  scale_fill_gradientn(colours = FI_SEQUENTIAL, ...)
}

# -------------------
# custom ggplot theme
# -------------------

# Figures are rendered at the exact size they occupy on the poster, so theme
# point sizes are real printed points: base_size is what the reader sees.
# ~18-22 pt carries 1-2 m; bump it for a larger figure box.
FI_BASE_SIZE <- 24

# geom text (annotate, geom_text) is sized in mm, theme text in pt.
fi_pt <- function(pt) pt / .pt

theme_poster <- function(base_size = FI_BASE_SIZE, base_family = FI_FONT) {
  half <- base_size / 2
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # transparent throughout -- safe on a white card and on the grey page
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      strip.background = element_blank(),
      panel.border = element_blank(),

      text = element_text(colour = FI_INK),
      axis.text = element_text(colour = FI_INK),
      # markdown-rendered so "SmO<sub>2</sub>" in the axis labels comes out
      # as a real subscript instead of relying on a font's own U+2082 glyph.
      # Set on .x/.y individually -- the generic axis.title doesn't reliably
      # cascade a non-element_text subclass down to them.
      axis.title.x = ggtext::element_markdown(colour = FI_INK),
      axis.title.y = ggtext::element_markdown(colour = FI_INK, angle = 90),
      strip.text = element_text(colour = FI_INK, face = "bold"),
      axis.line = element_line(colour = FI_INK, linewidth = 0.4),
      axis.ticks = element_line(colour = FI_INK, linewidth = 0.4),
      axis.ticks.length = unit(half / 4, "pt"),

      # major only, and only where a value has to be read off
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = FI_GRID, linewidth = 0.3),

      plot.title = ggtext::element_markdown(
        family = FI_FONT_TITLE,
        face = "bold",
        colour = FI_INK,
        margin = margin(b = half)
      ),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(half, half, half, half)
    )
}

theme_set(theme_poster())

# Lines and points have to hold up at 1-2 m as well: linewidth is in mm and is
# unaffected by base_size, so the defaults are raised once here.
update_geom_defaults("line", list(linewidth = 1, colour = FI_BLUE))
update_geom_defaults("segment", list(linewidth = 1, colour = FI_BLUE))
update_geom_defaults("point", list(size = 4, colour = FI_BLUE))
update_geom_defaults("bar", list(fill = FI_BLUE, colour = NA))
update_geom_defaults("col", list(fill = FI_BLUE, colour = NA))
update_geom_defaults(
  "text",
  list(colour = FI_INK, family = FI_FONT, size = fi_pt(FI_BASE_SIZE * 0.8))
)

# The size ONE panel occupies on the poster. Composites are sized as a
# multiple of this, so every panel on the poster is the same size and its text
# therefore prints at the same FI_BASE_SIZE points.
FI_FIG_W <- 21 # cm
FI_FIG_H <- 14 # cm

# Render ONCE at the size the figure occupies on the poster -- never export
# small and enlarge. Pixels follow from size x dpi.
save_poster <- function(plot, file, width, height, units = "cm", dpi = 300) {
  dir.create("Output", showWarnings = FALSE, recursive = TRUE)
  ggsave(
    filename = file.path("Output", file),
    plot = plot,
    device = ragg::agg_png,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = "transparent"
  )
}

# ------------
# Methods Plot
# ------------

# render the 8-panel breakpoints-method comparison for one session/channel
# and saves it as .png. Returns the saved file's path

make_methods_panel <- function(
  session = "ID07_01",
  channel = "smo2_dom",
  rater = "Rater_A",
  ncol = 2
) {
  SESSION <- session
  CHANNEL <- channel
  RATER <- rater
  PANEL_NCOL <- ncol

  COL_DATA <- FI_INK
  COL_FIT <- FI_BLUE
  COL_BP <- FI_GOLD
  COL_DERIV <- FI_NAVY
  # the two Dmax variants are told apart by colour rather than by a legend
  COL_VARIANT <- c(Dmax = FI_TEAL, ModDmax = FI_VIOLET)

  # Breakpoint labels normally hang from the top of the panel, just right of
  # their line. Where that collides with something else specific to one panel,
  # this pins a label to a fixed data-y and/or hjust instead -- keyed by panel
  # key, then by its label text. hjust 1.12 mirrors the default -0.12: the
  # label sits just to the LEFT of its line instead of just to the right.
  BP_LABEL_OVERRIDE <- list(
    poly = list(
      BP1 = list(y = 65),
      BP2 = list(y = 30)
    ),
    dmax_ratio = list(
      Dmax = list(y = 60, hjust = 1.12)
    ),
    dmax = list(
      # descending left to right, like panel F -- each label's height then
      # tracks its line's x-position, instead of Log-log (leftmost) landing
      # visually between the other two by accident of the default row stacking
      `Log-log` = list(y = 70, hjust = 1.12),
      Dmax = list(y = 50),
      ModDmax = list(y = 30)
    )
  )

  dat <- read.csv(file.path(
    "Data/IntermediateData",
    paste0(SESSION, "_preprocessed.csv")
  ))
  # Copy the channel into a fixed column so substitute() inside get_bp_*
  # captures the symbol ".y" rather than the variable name CHANNEL.
  dat[[".y"]] <- dat[[CHANNEL]]

  # ---- run every method ------------------------------------------------------
  # Listed in panel order.
  bps <- list(
    visual = get_bp_visual(dat, SESSION, CHANNEL, RATER, y = .y),
    raw = get_bp_raw(dat, y = .y),
    mean = get_bp_avg(dat, y = .y),
    poly = get_bp_poly(dat, y = .y),
    ratio = get_bp_ratio(dat, y = .y),
    dmax_ratio = get_bp_dmax_ratio(dat, y = .y),
    dmax = get_bp_dmax(dat, y = .y),
    slope = get_bp_slope(dat, y = .y)
  )

  method_meta <- tibble::tribble(
    ~key         , ~title                                , ~y_lab               , ~y_group ,
    "visual"     , "Visual identification"               , "SmO2 (%)"           , "pct"    ,
    "raw"        , "Double Piecewise (Raw signal)"       , "SmO2 (%)"           , "pct"    ,
    "mean"       , "Double Piecewise (Stage means)"      , "SmO2 (%)"           , "pct"    ,
    "poly"       , "Polynomial inflections"              , "SmO2 (%)"           , "pct"    ,
    "ratio"      , "Double Piecewise (Power/SmO2 ratio)" , "Power/SmO2 (W/%)"   , "ratio"  ,
    "dmax_ratio" , "Log-log & Dmax (Power/SmO2 ratio)"   , "Power/SmO2 (W/%)"   , "ratio"  ,
    "dmax"       , "Log-log & Dmax (Stage means)"        , "Deoxygenation (%)"  , "pct"    ,
    "slope"      , "Double Piecewise (Slope)"            , "SmO2 slope (%/min)" , "free"
  )

  # titles/y_labs above stay plain text to write
  method_meta$title <- gsub(
    "SmO2",
    "SmO<sub>2</sub>",
    method_meta$title,
    fixed = TRUE
  )
  method_meta$y_lab <- gsub(
    "SmO2",
    "SmO<sub>2</sub>",
    method_meta$y_lab,
    fixed = TRUE
  )

  # ---- shared x scaling ------------------------------------------------------
  # The time-domain panels keep their seconds axis, but the ramp is linear in
  # time, so converting the common power range back into seconds gives those
  # panels the *same* scaling: a breakpoint at a given wattage lands at the same
  # relative position in every panel, whatever the axis is labelled in.
  to_time <- function(power, ramp) {
    (power - ramp$sl) * ramp$step_length / ramp$li
  }

  x_range_w <- function(bp) {
    x <- bp$data$x
    if (!is.null(bp$ramp)) {
      x <- time_to_power(x, bp$ramp$sl, bp$ramp$li, bp$ramp$step_length)
    }
    range(x, na.rm = TRUE)
  }

  x_lim_w <- range(vapply(bps, x_range_w, numeric(2)))
  # room for the labels -- they are set in poster points, so the widest of them
  # ("ModDmax / 283 W") needs a fifth of the axis to its right
  x_lim_w <- x_lim_w + c(-0.05, 0.22) * diff(x_lim_w)

  panel_xlim <- function(bp) {
    if (is.null(bp$ramp)) x_lim_w else to_time(x_lim_w, bp$ramp)
  }

  # ---- shared y scaling ------------------------------------------------------
  # SmO2 and deoxygenation are percentages and always span the full 0-100.
  # The two power/SmO2 panels share one range so the ratio is comparable between
  # them; everything else is free.
  y_span <- function(bp) {
    v <- bp$data$y
    if (!is.null(bp$fit_line)) {
      v <- c(v, bp$fit_line$fitted)
    }
    if (!is.null(bp$chords)) {
      v <- c(v, bp$chords$y, bp$chords$yend)
    }
    range(v, na.rm = TRUE)
  }

  ratio_keys <- method_meta$key[method_meta$y_group == "ratio"]
  y_lim_ratio <- range(vapply(bps[ratio_keys], y_span, numeric(2)))
  y_lim_ratio[2] <- y_lim_ratio[2] + 0.30 * diff(y_lim_ratio) # label headroom

  panel_ylim <- function(group) {
    switch(group, pct = c(0, 100), ratio = y_lim_ratio, free = NULL)
  }

  # ---- one panel -------------------------------------------------------------
  plot_method <- function(key, tag) {
    m <- method_meta[method_meta$key == key, ]
    bp <- bps[[key]]
    d <- bp$data
    is_timeseries <- !is.null(bp$ramp)
    xlim <- panel_xlim(bp)
    ylim <- panel_ylim(m$y_group)
    title_txt <- paste0(tag, " - ", m$title)

    p <- ggplot(d, aes(x, y))

    # the raw signal is a continuous trace, every other method is stage-wise;
    # the trace goes down first so the fit reads on top of it
    if (is_timeseries) {
      p <- p + geom_line(colour = COL_DATA, alpha = 0.45, linewidth = 0.5)
    }

    # fitted model, drawn behind the stage points
    if (!is.null(bp$fit_line) && !all(is.na(bp$fit_line$fitted))) {
      p <- p +
        geom_line(
          data = bp$fit_line,
          aes(x = x, y = fitted),
          inherit.aes = FALSE,
          colour = COL_FIT,
          linewidth = 1.2
        )
    }

    # Dmax family: the point on the curve furthest from the chord each variant
    # is measured against. The chord itself is no longer drawn -- it only
    # cluttered the panel; the coloured marker alone says where each variant's
    # breakpoint falls.
    # if (!is.null(bp$drops)) {
    #   p <- p +
    #     geom_point(
    #       data = bp$drops,
    #       aes(x = x, y = y, colour = variant),
    #       inherit.aes = FALSE,
    #       size = 5
    #     )
    # }

    if (!is_timeseries) {
      p <- p + geom_point(colour = COL_DATA, alpha = 0.65, size = 3.5)
    }

    # ---- breakpoint markers ----
    # bp_x is the position on this panel's own axis (seconds for the time-domain
    # panels, watts elsewhere); power is always the wattage that gets printed.
    b <- bp$results[!is.na(bp$results$bp_x), ]
    rows <- 1
    if (nrow(b) > 0) {
      b <- b[order(b$bp_x), ]
      # panels showing several methods are labelled by method, not by BP number
      labels <- if (length(unique(b$method)) > 1) {
        sub("\\.ratio$", "", b$method)
      } else {
        paste0("BP", b$bp)
      }
      # stack labels of breakpoints that sit close together
      row <- cumsum(c(0, diff(b$bp_x) < 0.15 * diff(xlim)))
      rows <- max(row) + 1

      # a panel-specific override pins a label to a fixed data-y and/or moves
      # it to the other side of its line; vjust=0.5 centres the text once y is
      # a real data value instead of hanging from the panel top
      lab_y <- rep(Inf, nrow(b))
      lab_vjust <- 1.3 + 2.1 * row
      lab_hjust <- rep(-0.12, nrow(b)) # sit next to the line, not on it
      override <- BP_LABEL_OVERRIDE[[key]]
      for (lbl in names(override)) {
        hit <- which(labels == lbl)
        if (length(hit) == 0) {
          next
        }
        ov <- override[[lbl]]
        if (!is.null(ov$y)) {
          lab_y[hit] <- ov$y
          lab_vjust[hit] <- 0.5
        }
        if (!is.null(ov$hjust)) lab_hjust[hit] <- ov$hjust
      }

      p <- p +
        geom_vline(
          xintercept = b$bp_x,
          linetype = "dashed",
          linewidth = 0.9,
          colour = COL_BP
        ) +
        annotate(
          "text",
          x = b$bp_x,
          y = lab_y,
          label = paste0(labels, "\n", round(b$power), " W"),
          vjust = lab_vjust,
          hjust = lab_hjust,
          family = FI_FONT,
          size = fi_pt(FI_BASE_SIZE * 0.75),
          lineheight = 0.9,
          fontface = "bold",
          colour = COL_BP
        )
    }

    # ---- 2nd derivative, rescaled onto the primary axis ----
    # The polynomial breakpoints are its zeros, so it belongs on the same panel.
    # There is no secondary axis to carry its units -- it is unlabelled by
    # design, so a text tag next to the curve says what it is instead.
    if (!is.null(bp$deriv_line) && !all(is.na(bp$deriv_line$d2))) {
      d_rng <- range(bp$deriv_line$d2, na.rm = TRUE)
      span_y <- diff(ylim)
      span_d <- if (diff(d_rng) == 0) 1 else diff(d_rng)
      to_primary <- function(v) (v - d_rng[1]) / span_d * span_y + ylim[1]

      dl <- bp$deriv_line
      dl$y2 <- to_primary(dl$d2)
      if (to_primary(0) >= ylim[1] && to_primary(0) <= ylim[2]) {
        p <- p +
          geom_hline(
            yintercept = to_primary(0),
            colour = COL_DERIV,
            linetype = "dotted",
            linewidth = 0.7
          )
      }
      p <- p +
        geom_line(
          data = dl,
          aes(x = x, y = y2),
          inherit.aes = FALSE,
          colour = COL_DERIV,
          linewidth = 1
        )

      # tag at 90 % SmO2, at whichever of a few candidate x positions sits
      # furthest from the breakpoint labels -- a fixed height reads as a clear
      # legend entry rather than one that chases the curve around
      candidates <- xlim[1] + c(0.12, 0.5, 0.88) * diff(xlim)
      clearance <- function(x) {
        if (nrow(b) == 0) 0 else min(abs(x - b$bp_x))
      }
      tag_x <- candidates[which.max(vapply(candidates, clearance, numeric(1)))]
      p <- p +
        annotate(
          "text",
          x = tag_x,
          y = 90,
          label = "2nd derivative",
          vjust = 0.5,
          hjust = 0.5,
          family = FI_FONT,
          size = fi_pt(FI_BASE_SIZE * 0.7),
          fontface = "bold",
          colour = COL_DERIV
        )
    }

    x_lab <- if (is_timeseries) "Time (min:s)" else "Power (W)"

    # 10 min steps -- every second stage -- printed as m:ss. One tick per stage
    # collides at poster text sizes.
    if (is_timeseries) {
      p <- p +
        scale_x_continuous(
          breaks = seq(0, ceiling(max(xlim) / 600) * 600, by = 600),
          labels = function(t) sprintf("%d:%02d", t %/% 60, round(t %% 60))
        )
    }

    p <- p +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      scale_colour_manual(values = COL_VARIANT, guide = "none") +
      labs(x = x_lab, y = m$y_lab, title = title_txt)

    # free-scaled panels still need headroom for the stacked labels
    if (is.null(ylim)) {
      p <- p +
        scale_y_continuous(
          expand = expansion(mult = c(0.05, 0.06 + 0.12 * rows))
        )
    } else if (m$y_group == "pct") {
      p <- p + scale_y_continuous(breaks = seq(0, 100, by = 25))
    }
    p
  }

  figures <- setNames(
    Map(plot_method, method_meta$key, LETTERS[seq_len(nrow(method_meta))]),
    method_meta$key
  )

  PANEL_NCOL <- 2
  panel <- wrap_plots(figures, ncol = PANEL_NCOL)

  # preview only -- the pdf device the script would otherwise draw to cannot see
  # the OS fonts and would spray font warnings
  if (interactive()) {
    print(panel)
  }

  # create sizing
  PANEL_NROW <- ceiling(length(figures) / PANEL_NCOL)

  save_poster(
    panel,
    "Fig_Panel.png",
    width = FI_FIG_W * PANEL_NCOL,
    height = FI_FIG_H * PANEL_NROW
  )

  file.path("Output", "Fig_Panel.png")
}

#-----------------
# Reliability Plot
#-----------------

make_reliability_panel <- function(
  icc_rds = file.path("Data", "AnalysisData", "icc_all.rds")
) {
  icc_all <- readRDS(icc_rds)

  # manually define one method to highlight for BP1 and BP2
  HIGHLIGHT_BP1 <- list(channel = "AVG", method = "Log-log.ratio")
  HIGHLIGHT_BP2 <- list(channel = "AVG", method = "Dmax.ratio")
  # also highlight the visual method
  HIGHLIGHT_VISUAL <- list(channel = "AVG", method = "visual_Rater_A")

  # positioning the text labels
  REL_XLIM <- c(0, 0.15)
  REL_LABEL_X <- REL_XLIM[2] * 0.99

  # create a function to generate the plots for BP1 and BP2 as
  # they share mostly similar characteristics
  make_rel_plot <- function(bp_num, tag, highlight, n = 10) {
    d <- icc_all |>
      filter(bp == bp_num) |>
      arrange(cv) |>
      head(n) |>
      mutate(
        method_label = sub(
          "_Rater_.*$",
          "",
          sub("^(DOM|NDOM|AVG)_", "", as.character(combination))
        ),
        reli_label = sprintf(
          "%.2f [%.2f, %.2f]; %.0f W",
          icc,
          icc_lower,
          icc_upper,
          sem
        ),
        highlight = dplyr::case_when(
          channel == highlight$channel & method == highlight$method ~ "best",
          channel == HIGHLIGHT_VISUAL$channel &
            method == HIGHLIGHT_VISUAL$method ~ "visual",
          TRUE ~ "other"
        ),
        combination = tidytext::reorder_within(combination, -cv, bp)
      )

    ggplot(d, aes(x = cv, y = combination)) +
      geom_col_pattern(
        aes(fill = highlight, pattern = channel),
        colour = NA,
        pattern_fill = FI_GRID,
        pattern_colour = NA,
        pattern_density = 0.15,
        pattern_spacing = 0.03,
        pattern_angle = 45
      ) +
      geom_text(
        aes(label = method_label),
        x = REL_LABEL_X,
        hjust = 1,
        family = FI_FONT,
        size = fi_pt(FI_BASE_SIZE * 0.75),
        colour = FI_INK
      ) +
      geom_text(
        aes(y = combination, label = reli_label),
        x = 0.001,
        hjust = 0,
        inherit.aes = FALSE,
        family = FI_FONT,
        size = fi_pt(FI_BASE_SIZE * 0.65),
        colour = FI_INK
      ) +
      scale_x_continuous(limits = REL_XLIM, labels = scales::percent) +
      scale_fill_manual(
        values = c(other = "grey70", best = FI_GOLD, visual = FI_TEAL),
        guide = "none"
      ) +
      scale_pattern_manual(
        values = c(DOM = "stripe", NDOM = "crosshatch", AVG = "none")
      ) +
      labs(
        x = "Coefficient of Variation",
        y = NULL,
        title = paste0(tag, " - BP", bp_num)
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank()
      )
  }

  # formatting for the custom panel figure
  label_patch <- ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0,
      label = "Coefficient of Variation (%)",
      hjust = 0.5,
      family = FI_FONT,
      colour = FI_INK,
      size = fi_pt(FI_BASE_SIZE)
    ) +
    theme_void()

  # now create the plot(s)
  p_bp1 <- make_rel_plot(1, "A", HIGHLIGHT_BP1)
  p_bp2 <- make_rel_plot(2, "B", HIGHLIGHT_BP2)

  p_rel <- (p_bp1 + labs(x = NULL)) +
    (p_bp2 + labs(x = NULL)) +
    label_patch +
    patchwork::guide_area() +
    plot_layout(
      guides = "collect",
      design = "AB\nCD",
      heights = c(1, 0.08)
    ) &
    theme(legend.position = "bottom")

  ggsave(
    filename = file.path("Output", "Fig_Rel.png"),
    plot = p_rel,
    device = ragg::agg_png,
    width = 30,
    height = 21,
    units = "cm",
    dpi = 300,
    bg = "transparent"
  )
  file.path("Output", "Fig_Rel.png")
}

# Renders both poster figures. This is what the targets pipeline calls.
make_poster_figures <- function(
  session = "ID07_01",
  channel = "smo2_dom",
  rater = "Rater_A"
) {
  c(
    make_methods_panel(session, channel, rater),
    make_reliability_panel()
  )
}
