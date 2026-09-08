# Manual SmO2 Breakpoint Rating App
#
# Run from the project root directory:
#   shiny::runApp("Scripts/ManualRating/manual_rating_app.R")
#
# Output: Data/ManualRatings/<rater_name>_ratings.csv
# Columns: rater, session_id, channel, method, bp, bp_x, power, identifiable
#   - method = "visual"
#   - bp_x   = time in seconds from exercise start
#   - power  = watts (looked up from the stage power at that time)
#   - identifiable = TRUE / FALSE / NA (NA = not yet decided)

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tibble)

.default_data_dir <- getwd()

# ── Constants ────────────────────────────────────────────────────────────────

CHANNELS <- c("smo2_dom", "smo2_ndom", "smo2_avg")
CH_LABEL <- c(
  smo2_dom = "Dominant leg",
  smo2_ndom = "Non-dominant leg",
  smo2_avg = "Average"
)
BP_COLOR <- c("1" = "#1565C0", "2" = "#B71C1C")

NI_MAP <- list(
  ni_dom_1 = list(ch = "smo2_dom", n = 1L),
  ni_dom_2 = list(ch = "smo2_dom", n = 2L),
  ni_ndom_1 = list(ch = "smo2_ndom", n = 1L),
  ni_ndom_2 = list(ch = "smo2_ndom", n = 2L),
  ni_avg_1 = list(ch = "smo2_avg", n = 1L),
  ni_avg_2 = list(ch = "smo2_avg", n = 2L)
)

# ── Data helpers ─────────────────────────────────────────────────────────────

sessions_from_dir <- function(data_dir) {
  sort(sub(
    "_preprocessed$",
    "",
    tools::file_path_sans_ext(basename(
      list.files(data_dir, pattern = "_preprocessed\\.csv$", full.names = FALSE)
    ))
  ))
}

read_session_data <- function(sid, data_dir) {
  dat <- readr::read_csv(
    file.path(data_dir, paste0(sid, "_preprocessed.csv")),
    show_col_types = FALSE
  )
  dat[!is.na(dat$time), ] # keep full recording including recovery
}

exercise_end_t <- function(dat) {
  max(dat$time[!is.na(dat$lap)], na.rm = TRUE)
}

get_stage_info <- function(dat) {
  dat |>
    dplyr::filter(!is.na(lap)) |>
    dplyr::group_by(lap) |>
    dplyr::summarize(
      t_start = min(time, na.rm = TRUE),
      power_w = round(mean(power, na.rm = TRUE)),
      .groups = "drop"
    )
}

# Same linear ramp formula as get_bp_raw() / time_to_power():
#   power = starting_load - load_increase + t * (load_increase / step_length)
time_to_power_ramp <- function(t, dat, step_length = 300) {
  if (is.null(t) || is.na(t)) {
    return(NA_real_)
  }
  sl <- mean(dat$power[!is.na(dat$lap) & dat$lap == 1L], na.rm = TRUE)
  li <- mean(dat$power[!is.na(dat$lap) & dat$lap == 2L], na.rm = TRUE) - sl
  sl + t * li / step_length
}

# ── Ratings I/O ───────────────────────────────────────────────────────────────

ratings_path <- function(rater, ratings_dir) {
  file.path(ratings_dir, paste0(rater, "_ratings.csv"))
}

empty_ratings <- function() {
  tibble::tibble(
    rater = character(),
    session_id = character(),
    channel = character(),
    method = character(),
    bp = integer(),
    bp_x = numeric(),
    power = numeric(),
    identifiable = logical(),
    bp2_type = character() # "flat" | "deoxy" | NA; only for bp == 2
  )
}

load_ratings <- function(rater, ratings_dir) {
  p <- ratings_path(rater, ratings_dir)
  if (file.exists(p)) {
    readr::read_csv(p, show_col_types = FALSE)
  } else {
    empty_ratings()
  }
}

save_ratings <- function(df, rater, ratings_dir) {
  dir.create(ratings_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, ratings_path(rater, ratings_dir))
}

has_rating <- function(sid, ratings) {
  sid %in% ratings$session_id[ratings$method == "visual"]
}

init_bps <- function() {
  setNames(
    lapply(CHANNELS, function(x) list(bp1 = NA_real_, bp2 = NA_real_)),
    CHANNELS
  )
}

bps_from_ratings <- function(sid, ratings) {
  bps <- init_bps()
  r <- ratings[ratings$session_id == sid & ratings$method == "visual", ]
  if (nrow(r) == 0) {
    return(bps)
  }
  for (ch in CHANNELS) {
    for (n in 1:2) {
      row <- r[r$channel == ch & r$bp == n, ]
      if (nrow(row) == 1 && isTRUE(row$identifiable) && !is.na(row$bp_x)) {
        bps[[ch]][[paste0("bp", n)]] <- row$bp_x
      }
    }
  }
  bps
}

ni_was_saved <- function(sid, ch, n, ratings) {
  r <- ratings[
    ratings$session_id == sid &
      ratings$channel == ch &
      ratings$bp == n &
      ratings$method == "visual",
  ]
  nrow(r) == 1 && isFALSE(r$identifiable[1])
}

ni_input_id <- function(ch, n) {
  switch(
    paste0(ch, "_", n),
    smo2_dom_1 = "ni_dom_1",
    smo2_dom_2 = "ni_dom_2",
    smo2_ndom_1 = "ni_ndom_1",
    smo2_ndom_2 = "ni_ndom_2",
    smo2_avg_1 = "ni_avg_1",
    smo2_avg_2 = "ni_avg_2"
  )
}

bp2_type_input_id <- function(ch) {
  switch(
    ch,
    smo2_dom = "bp2type_dom",
    smo2_ndom = "bp2type_ndom",
    smo2_avg = "bp2type_avg"
  )
}

# ── Plot ─────────────────────────────────────────────────────────────────────

draw_smo2_plot <- function(dat, ch, bps, stages, exercise_end, show_x = FALSE) {
  # Y-range from exercise data only; recovery can exceed range and be clipped
  y_ex <- dat[[ch]][!is.na(dat$lap)]
  y_rng <- range(y_ex, na.rm = TRUE)
  y_pad <- max(diff(y_rng) * 0.10, 1)
  y_lim <- c(y_rng[1] - y_pad, y_rng[2] + y_pad)
  y_span <- diff(y_lim)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = time, y = .data[[ch]])) +
    ggplot2::geom_point(
      shape = 21,
      fill = "white",
      colour = "black",
      size = 1.2,
      stroke = 0.35
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.005, 0.02))
    ) +
    ggplot2::coord_cartesian(ylim = y_lim) +
    ggplot2::labs(
      title = CH_LABEL[[ch]],
      x = if (show_x) "Time (s)" else NULL,
      y = "SmO2 (%)"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.text.x = if (!show_x) ggplot2::element_blank() else NULL,
      axis.ticks.x = if (!show_x) ggplot2::element_blank() else NULL,
      plot.margin = ggplot2::margin(2, 8, 2, 2)
    )

  # Stage boundaries
  if (nrow(stages) > 0) {
    p <- p +
      ggplot2::geom_vline(
        data = stages,
        mapping = ggplot2::aes(xintercept = t_start),
        colour = "grey78",
        linetype = "dotted",
        linewidth = 0.4
      ) +
      ggplot2::annotate(
        "text",
        x = stages$t_start + 6,
        y = y_lim[2] - y_span * 0.04,
        label = paste0(stages$power_w, "W"),
        hjust = 0,
        vjust = 1,
        size = 2.5,
        colour = "grey55"
      )
  }

  # End-of-exercise / start-of-recovery line
  if (!is.null(exercise_end) && !is.na(exercise_end)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = exercise_end,
        colour = "grey20",
        linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text",
        x = exercise_end + diff(range(dat$time, na.rm = TRUE)) * 0.005,
        y = y_lim[2] - y_span * 0.04,
        label = "Recovery",
        hjust = 0,
        vjust = 1,
        size = 2.5,
        colour = "grey30",
        fontface = "italic"
      )
  }

  # BP markers
  for (n in 1:2) {
    t <- bps[[ch]][[paste0("bp", n)]]
    if (!is.null(t) && !is.na(t)) {
      col <- BP_COLOR[[as.character(n)]]
      pw <- time_to_power_ramp(t, dat)
      y_lab <- y_lim[2] - y_span * (if (n == 1L) 0.08 else 0.28)
      p <- p +
        ggplot2::geom_vline(
          xintercept = t,
          colour = col,
          linewidth = 1.0,
          linetype = "dashed"
        ) +
        ggplot2::annotate(
          "text",
          x = t,
          y = y_lab,
          label = paste0("BP", n, "\n", round(pw), "W"),
          colour = col,
          hjust = -0.08,
          vjust = 1,
          size = 3.2,
          fontface = "bold"
        )
    }
  }
  p
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("SmO₂ Breakpoints – Manual Rating"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Rater
      uiOutput("rater_panel"),
      hr(),

      # Session navigation
      uiOutput("nav_panel"),
      hr(),

      # Click mode
      radioButtons(
        "click_mode",
        "Currently placing:",
        choices = c(
          "BP1  (1st threshold)" = "1",
          "BP2  (2nd threshold)" = "2"
        ),
        selected = "1"
      ),
      helpText("Click to place. Click near an existing marker to move it."),
      actionButton(
        "clear_bps",
        "Clear this session",
        class = "btn-sm btn-warning"
      ),
      hr(),

      # Not identifiable checkboxes
      h5("Mark as not identifiable:"),
      fluidRow(
        column(
          4,
          tags$b("Dom"),
          checkboxInput("ni_dom_1", "BP1", FALSE),
          checkboxInput("ni_dom_2", "BP2", FALSE)
        ),
        column(
          4,
          tags$b("NDom"),
          checkboxInput("ni_ndom_1", "BP1", FALSE),
          checkboxInput("ni_ndom_2", "BP2", FALSE)
        ),
        column(
          4,
          tags$b("Avg"),
          checkboxInput("ni_avg_1", "BP1", FALSE),
          checkboxInput("ni_avg_2", "BP2", FALSE)
        )
      ),
      hr(),

      # BP2 type
      h5("BP2 type:"),
      fluidRow(
        column(4, tags$b("Dom")),
        column(
          8,
          radioButtons(
            "bp2type_dom",
            NULL,
            choices = c("Flat" = "flat", "Deoxy" = "deoxy"),
            selected = "flat",
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(4, tags$b("NDom")),
        column(
          8,
          radioButtons(
            "bp2type_ndom",
            NULL,
            choices = c("Flat" = "flat", "Deoxy" = "deoxy"),
            selected = "flat",
            inline = TRUE
          )
        )
      ),
      fluidRow(
        column(4, tags$b("Avg")),
        column(
          8,
          radioButtons(
            "bp2type_avg",
            NULL,
            choices = c("Flat" = "flat", "Deoxy" = "deoxy"),
            selected = "flat",
            inline = TRUE
          )
        )
      ),
      hr(),

      # Live placement summary
      h5("Placements:"),
      tableOutput("bp_summary"),
      hr(),

      actionButton(
        "save_btn",
        "Save & Next ➤",
        class = "btn-success btn-block"
      ),
      br(),
      tags$small(textOutput("save_msg"), style = "color:#2e7d32;")
    ),

    mainPanel(
      width = 9,
      plotOutput("plot_dom", click = "click_dom", height = "215px"),
      plotOutput("plot_ndom", click = "click_ndom", height = "215px"),
      plotOutput("plot_avg", click = "click_avg", height = "215px")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  rv <- reactiveValues(
    rater = NULL,
    data_dir = NULL,
    ratings_dir = NULL,
    all_sessions = character(0),
    session_idx = 1L,
    ratings = empty_ratings(),
    bps = init_bps(),
    save_msg = ""
  )

  sess_data <- reactiveVal(NULL)
  stage_data <- reactiveVal(data.frame())
  ex_end <- reactiveVal(NA_real_)

  # ── Rater setup ─────────────────────────────────────────────────────────────

  output$rater_panel <- renderUI({
    if (!is.null(rv$rater)) {
      n_rated <- sum(vapply(
        rv$all_sessions,
        has_rating,
        logical(1),
        rv$ratings
      ))
      tagList(
        tags$b("Rater: "),
        tags$span(rv$rater, style = "color:#1565C0; font-size:1.05em;"),
        br(),
        tags$small(paste0(
          n_rated,
          " / ",
          length(rv$all_sessions),
          " sessions rated"
        )),
        br(),
        tags$small(basename(rv$data_dir), style = "color:grey;")
      )
    } else {
      tagList(
        textInput("rater_input", "Your name:", placeholder = "e.g. Rater_A"),
        tags$label("Data folder:"),
        fluidRow(
          column(
            8,
            textInput(
              "data_dir_input",
              NULL,
              value = .default_data_dir,
              placeholder = "Full path to folder with *_preprocessed.csv files"
            )
          ),
          column(
            4,
            actionButton("browse_btn", "Browse…", style = "margin-top:0px;")
          )
        ),
        helpText("Ratings are saved next to this folder in ManualRatings/."),
        actionButton(
          "start_btn",
          "Start Rating",
          class = "btn-primary btn-block"
        ),
        uiOutput("start_error")
      )
    }
  })

  output$start_error <- renderUI(NULL)

  observeEvent(input$browse_btn, {
    path <- choose.dir(
      default = input$data_dir_input,
      caption = "Select data folder"
    )
    if (!is.na(path)) {
      updateTextInput(session, "data_dir_input", value = path)
    }
  })

  observeEvent(input$start_btn, {
    nm <- trimws(input$rater_input)
    data_dir <- normalizePath(trimws(input$data_dir_input), mustWork = FALSE)

    if (nchar(nm) == 0) {
      output$start_error <- renderUI(tags$p(
        "Please enter your name.",
        style = "color:red;"
      ))
      return()
    }
    if (!dir.exists(data_dir)) {
      output$start_error <- renderUI(tags$p(
        "Data folder not found.",
        style = "color:red;"
      ))
      return()
    }
    sessions <- sessions_from_dir(data_dir)
    if (length(sessions) == 0) {
      output$start_error <- renderUI(
        tags$p(
          "No *_preprocessed.csv files found in that folder.",
          style = "color:red;"
        )
      )
      return()
    }

    rv$data_dir <- data_dir
    rv$ratings_dir <- file.path(dirname(data_dir), "ManualRatings")
    rv$all_sessions <- sessions
    rv$rater <- nm
    rv$ratings <- load_ratings(nm, rv$ratings_dir)
    output$start_error <- renderUI(NULL)
    goto_session(1L)
  })

  # ── Session loading ──────────────────────────────────────────────────────────

  goto_session <- function(idx) {
    rv$session_idx <- idx
    sid <- rv$all_sessions[idx]

    dat <- read_session_data(sid, rv$data_dir)
    sess_data(dat)
    stage_data(get_stage_info(dat))
    ex_end(exercise_end_t(dat))

    rv$bps <- bps_from_ratings(sid, rv$ratings)

    for (ni_id in names(NI_MAP)) {
      m <- NI_MAP[[ni_id]]
      val <- ni_was_saved(sid, m$ch, m$n, rv$ratings)
      updateCheckboxInput(session, ni_id, value = val)
    }

    # Restore bp2_type radio buttons
    for (ch in CHANNELS) {
      saved_type <- "flat" # default
      if ("bp2_type" %in% names(rv$ratings) && nrow(rv$ratings) > 0) {
        r <- rv$ratings[
          rv$ratings$session_id == sid &
            rv$ratings$channel == ch &
            rv$ratings$bp == 2L &
            rv$ratings$method == "visual",
        ]
        if (nrow(r) == 1 && !is.na(r$bp2_type[1])) saved_type <- r$bp2_type[1]
      }
      updateRadioButtons(session, bp2_type_input_id(ch), selected = saved_type)
    }

    updateRadioButtons(session, "click_mode", selected = "1")
    updateSelectInput(session, "session_jump", selected = as.character(idx))
  }

  # ── Navigation UI ─────────────────────────────────────────────────────────

  output$nav_panel <- renderUI({
    req(rv$rater)
    idx <- rv$session_idx
    sid <- rv$all_sessions[idx]
    nsid <- length(rv$all_sessions)

    session_labels <- vapply(
      seq_along(rv$all_sessions),
      function(i) {
        s <- rv$all_sessions[i]
        paste0(s, if (has_rating(s, rv$ratings)) " ✓" else "")
      },
      character(1)
    )

    tagList(
      tags$b(paste0("Session ", idx, " / ", nsid, ": ", sid)),
      if (has_rating(sid, rv$ratings)) {
        tags$span(" (saved)", style = "color:green; font-size:0.85em;")
      } else {
        tags$span(" (unsaved)", style = "color:grey; font-size:0.85em;")
      },
      br(),
      br(),
      fluidRow(
        column(5, actionButton("btn_prev", "← Prev", width = "100%")),
        column(2),
        column(
          5,
          actionButton(
            "btn_next",
            "Next →",
            class = "btn-primary",
            width = "100%"
          )
        )
      ),
      br(),
      selectInput(
        "session_jump",
        "Jump to:",
        choices = setNames(
          as.character(seq_along(rv$all_sessions)),
          session_labels
        ),
        selected = as.character(idx)
      )
    )
  })

  observeEvent(input$btn_prev, {
    req(rv$rater)
    if (rv$session_idx > 1L) goto_session(rv$session_idx - 1L)
  })

  observeEvent(input$btn_next, {
    req(rv$rater)
    save_current_session()
    if (rv$session_idx < length(rv$all_sessions)) {
      goto_session(rv$session_idx + 1L)
    }
  })

  observeEvent(
    input$session_jump,
    {
      req(rv$rater)
      idx <- suppressWarnings(as.integer(input$session_jump))
      if (!is.na(idx) && idx != rv$session_idx) goto_session(idx)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  # ── Click handlers ──────────────────────────────────────────────────────────

  place_bp <- function(click_val, ch) {
    req(click_val, sess_data())
    dat <- sess_data()
    end_t <- ex_end()
    t <- max(0, min(as.numeric(click_val$x), end_t)) # clamp to exercise
    threshold <- end_t * 0.025 # 2.5% of exercise duration = "near a marker"

    bp1 <- rv$bps[[ch]]$bp1
    bp2 <- rv$bps[[ch]]$bp2
    near1 <- !is.na(bp1) && abs(t - bp1) < threshold
    near2 <- !is.na(bp2) && abs(t - bp2) < threshold

    n <- if (near1 || near2) {
      # Move the closest existing marker
      if (near1 && near2) {
        if (abs(t - bp1) <= abs(t - bp2)) 1L else 2L
      } else if (near1) {
        1L
      } else {
        2L
      }
    } else if (is.na(bp1)) {
      1L # always place BP1 first on this channel
    } else if (is.na(bp2)) {
      2L # then BP2
    } else {
      as.integer(input$click_mode) # both placed — honour mode selector
    }

    bps <- rv$bps
    bps[[ch]][[paste0("bp", n)]] <- t
    rv$bps <- bps

    updateCheckboxInput(session, ni_input_id(ch, n), value = FALSE)
    updateRadioButtons(session, "click_mode", selected = as.character(n))
  }

  observeEvent(input$click_dom, {
    place_bp(input$click_dom, "smo2_dom")
  })
  observeEvent(input$click_ndom, {
    place_bp(input$click_ndom, "smo2_ndom")
  })
  observeEvent(input$click_avg, {
    place_bp(input$click_avg, "smo2_avg")
  })

  # ── Not-identifiable checkboxes ─────────────────────────────────────────────

  for (ni_id in names(NI_MAP)) {
    local({
      id <- ni_id
      m <- NI_MAP[[id]]
      observeEvent(
        input[[id]],
        {
          if (isTRUE(input[[id]])) {
            bps <- rv$bps
            bps[[m$ch]][[paste0("bp", m$n)]] <- NA_real_
            rv$bps <- bps
            if (m$n == 2L) {
              updateRadioButtons(
                session,
                bp2_type_input_id(m$ch),
                selected = "flat"
              )
            }
          }
        },
        ignoreInit = TRUE
      )
    })
  }

  # ── Clear session ──────────────────────────────────────────────────────────

  observeEvent(input$clear_bps, {
    rv$bps <- init_bps()
    for (id in names(NI_MAP)) {
      updateCheckboxInput(session, id, value = FALSE)
    }
    for (ch in CHANNELS) {
      updateRadioButtons(session, bp2_type_input_id(ch), selected = "flat")
    }
  })

  # ── Save helper (used by save_btn and btn_next) ───────────────────────────

  save_current_session <- function() {
    req(rv$rater, sess_data())
    sid <- rv$all_sessions[rv$session_idx]
    dat <- sess_data()

    rows <- do.call(
      rbind,
      lapply(CHANNELS, function(ch) {
        do.call(
          rbind,
          lapply(1:2, function(n) {
            ni_id <- ni_input_id(ch, n)
            is_ni <- isTRUE(input[[ni_id]])
            t_val <- rv$bps[[ch]][[paste0("bp", n)]]
            has_t <- !is.null(t_val) && !is.na(t_val)

            type_raw <- input[[bp2_type_input_id(ch)]]
            tibble::tibble(
              rater = rv$rater,
              session_id = sid,
              channel = ch,
              method = "visual",
              bp = as.integer(n),
              bp_x = if (has_t && !is_ni) t_val else NA_real_,
              power = if (has_t && !is_ni) {
                time_to_power_ramp(t_val, dat)
              } else {
                NA_real_
              },
              identifiable = if (is_ni) {
                FALSE
              } else if (has_t) {
                TRUE
              } else {
                NA
              },
              bp2_type = if (n == 2L && has_t && !is_ni) {
                type_raw
              } else {
                NA_character_
              }
            )
          })
        )
      })
    )

    keep <- !(rv$ratings$session_id == sid & rv$ratings$method == "visual")
    rv$ratings <- dplyr::bind_rows(rv$ratings[keep, ], rows)
    save_ratings(rv$ratings, rv$rater, rv$ratings_dir)
    rv$save_msg <- paste0("Saved at ", format(Sys.time(), "%H:%M:%S"))
  }

  # ── Save handler ─────────────────────────────────────────────────────────────

  observeEvent(input$save_btn, {
    save_current_session()
  })

  output$save_msg <- renderText(rv$save_msg)

  # ── BP summary table ────────────────────────────────────────────────────────

  output$bp_summary <- renderTable(
    {
      dat <- sess_data()
      if (is.null(dat)) {
        return(NULL)
      }

      do.call(
        rbind,
        lapply(CHANNELS, function(ch) {
          fmt_bp <- function(n) {
            ni_id <- ni_input_id(ch, n)
            t_val <- rv$bps[[ch]][[paste0("bp", n)]]
            if (isTRUE(input[[ni_id]])) {
              "N/I"
            } else if (!is.null(t_val) && !is.na(t_val)) {
              pw_str <- paste0(
                round(t_val),
                "s / ",
                round(time_to_power_ramp(t_val, dat)),
                "W"
              )
              if (n == 2L) {
                type_raw <- input[[bp2_type_input_id(ch)]]
                type_tag <- switch(
                  type_raw,
                  flat = " [Flat]",
                  deoxy = " [Deoxy]",
                  ""
                )
                paste0(pw_str, type_tag)
              } else {
                pw_str
              }
            } else {
              "—"
            }
          }
          data.frame(
            Channel = CH_LABEL[[ch]],
            BP1 = fmt_bp(1),
            BP2 = fmt_bp(2),
            stringsAsFactors = FALSE
          )
        })
      )
    },
    striped = TRUE,
    bordered = TRUE,
    rownames = FALSE
  )

  # ── Plots ────────────────────────────────────────────────────────────────────

  make_plot_output <- function(ch, show_x = FALSE) {
    renderPlot({
      dat <- sess_data()
      if (is.null(dat)) {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "Enter your name to start",
            size = 5,
            colour = "grey60"
          )
      } else {
        draw_smo2_plot(dat, ch, rv$bps, stage_data(), ex_end(), show_x)
      }
    })
  }

  output$plot_dom <- make_plot_output("smo2_dom", show_x = FALSE)
  output$plot_ndom <- make_plot_output("smo2_ndom", show_x = FALSE)
  output$plot_avg <- make_plot_output("smo2_avg", show_x = TRUE)
}

shinyApp(ui, server)
