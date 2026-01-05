# app.R
# Generic Shiny Online Judge for R coding contests
# - Problems are loaded from ./problems/<problem_id>/problem.R
# - Each problem defines:
#     problem <- list(
#       id, title, description, function_name,
#       template_code, time_limit_sec, make_tests, (optional) compare
#     )

library(shiny)
library(readr)
library(DT)

# ----- Scoreboard helpers (opt-in ranking) -----

scores_file <- "scores.rds"
admin_password <- Sys.getenv("SCORES_ADMIN_PASSWORD", unset = "changeme")

empty_scores <- function() {
  data.frame(
    problem_id    = character(),
    problem_title = character(),
    user_name     = character(),
    status        = character(),
    passed        = integer(),
    total         = integer(),
    elapsed       = numeric(),
    timestamp     = as.POSIXct(character()),
    source_code   = character(),          # ADDED
    stringsAsFactors = FALSE
  )
}

load_scores <- function() {
  if (!file.exists(scores_file)) return(empty_scores())
  out <- try(readRDS(scores_file), silent = TRUE)
  if (inherits(out, "try-error")) return(empty_scores())

  # Backward compatibility: old scores.rds won't have source_code
  if (!("source_code" %in% names(out))) out$source_code <- NA_character_

  out
}

save_scores <- function(df) {
  saveRDS(df, scores_file)
}

# ----- Helper: load problems from ./problems -----

load_problems <- function(problems_dir = "problems") {
  if (!dir.exists(problems_dir)) {
    warning("Problems directory '", problems_dir, "' does not exist.")
    return(list())
  }

  problem_dirs <- list.dirs(problems_dir, full.names = TRUE, recursive = FALSE)
  problems <- list()

  for (dir in problem_dirs) {
    problem_file <- file.path(dir, "problem.R")
    if (!file.exists(problem_file)) next

    env <- new.env(parent = baseenv())
    tryCatch(
      {
        sys.source(problem_file, envir = env)
        if (exists("problem", envir = env, inherits = FALSE)) {
          p <- get("problem", envir = env)
          if (!is.null(p$id)) {
            problems[[p$id]] <- p
          }
        }
      },
      error = function(e) {
        warning("Failed to load problem from ", problem_file, ": ", conditionMessage(e))
      }
    )
  }

  problems
}

# ----- Helper: run tests for a given problem -----

run_all_tests <- function(fun, problem) {
  if (is.null(problem$make_tests) || !is.function(problem$make_tests)) {
    stop("Problem does not define a valid make_tests() function.")
  }

  tests <- problem$make_tests()
  total <- length(tests)
  if (total == 0L) {
    stop("Problem has no tests.")
  }

  time_limit <- if (!is.null(problem$time_limit_sec)) problem$time_limit_sec else 2
  passed <- 0L
  wrong_cases <- list()
  status <- "AC"

  # Optional comparison function (for approximate numeric equality, etc.)
  compare_fun <- problem$compare
  if (is.null(compare_fun) || !is.function(compare_fun)) {
    compare_fun <- function(x, y) identical(x, y)
  }

  t <- system.time({
    for (i in seq_along(tests)) {
      tc <- tests[[i]]
      res <- try(do.call(fun, tc$input), silent = TRUE)

      if (inherits(res, "try-error")) {
        status <- "RE"
        wrong_cases[[length(wrong_cases) + 1L]] <- list(
          id = i,
          input = tc$input,
          expected = tc$expected,
          got = "ERROR"
        )
        next
      }

      ok <- FALSE
      cmp <- try(compare_fun(res, tc$expected), silent = TRUE)
      if (!inherits(cmp, "try-error") &&
          is.logical(cmp) && length(cmp) == 1L && isTRUE(cmp)) {
        ok <- TRUE
      }

      if (ok) {
        passed <- passed + 1L
      } else {
        if (status == "AC") status <- "WA"
        wrong_cases[[length(wrong_cases) + 1L]] <- list(
          id = i,
          input = tc$input,
          expected = tc$expected,
          got = res
        )
      }
    }
  })

  elapsed <- as.numeric(t[["elapsed"]])

  if (passed == total && elapsed > time_limit) {
    status <- "TLE"
  } else if (passed == total && status == "AC") {
    status <- "AC"
  }

  list(
    status = status,
    passed = passed,
    total = total,
    elapsed = elapsed,
    wrong_cases = wrong_cases
  )
}

parse_mmss_to_seconds <- function(x) {
  # Accept "MM:SS" or "MM:SS.s" or "HH:MM:SS.s"
  x <- trimws(x)
  parts <- strsplit(x, ":", fixed = TRUE)

  out <- rep(NA_real_, length(parts))
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (length(p) == 2) {
      mm <- suppressWarnings(as.numeric(p[1]))
      ss <- suppressWarnings(as.numeric(p[2]))
      if (is.finite(mm) && is.finite(ss)) out[i] <- mm * 60 + ss
    } else if (length(p) == 3) {
      hh <- suppressWarnings(as.numeric(p[1]))
      mm <- suppressWarnings(as.numeric(p[2]))
      ss <- suppressWarnings(as.numeric(p[3]))
      if (is.finite(hh) && is.finite(mm) && is.finite(ss)) out[i] <- hh * 3600 + mm * 60 + ss
    }
  }
  out
}

parse_timestamp_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))

  if (is.numeric(x)) {
    # treat as epoch seconds if it looks like it
    if (all(x > 1e8, na.rm = TRUE)) {
      return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    }
    # otherwise treat as seconds-from-midnight on 1970-01-01
    base <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
    return(base + x)
  }

  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  # Strip leading apostrophe used to force Excel text
  x <- sub("^'", "", x)

  # First: try parsing as datetime (common formats)
  out <- suppressWarnings(readr::parse_datetime(x))

  # Fallback: if it looks like mm:ss(.), parse as duration and anchor to 1970-01-01
  still_na <- is.na(out) & !is.na(x)
  if (any(still_na)) {
    secs <- parse_mmss_to_seconds(x[still_na])
    if (any(!is.na(secs))) {
      base <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
      out[still_na] <- base + secs
    }
  }

  as.POSIXct(out, tz = "UTC")
}


# Load all problems at startup
problems <- load_problems()

problem_choices <- if (length(problems) > 0) {
  titles <- vapply(problems, function(p) p$title, character(1))
  # names = labels (shown), values = ids (internal)
  setNames(names(problems), titles)
} else {
  character(0)
}

# ----- UI -----

ui <- fluidPage(
  # Wrap problem statement text instead of horizontal scrolling
  tags$head(
    tags$style(HTML("
      #problem_description {
        white-space: pre-wrap;
        word-wrap: break-word;
        overflow-wrap: break-word;
      }
    "))
  ),

  titlePanel("R Coding Contest - Online Judge"),

  sidebarLayout(
    sidebarPanel(
      h4("Problem selection"),
      selectInput(
        inputId = "problem_id",
        label = "Choose a problem:",
        choices = problem_choices
      ),
      hr(),
      h4("Problem statement"),
      verbatimTextOutput("problem_description"),
      hr(),
      actionButton("load_template", "Load / Reset template"),
      br(), br(),
      actionButton("run_judge", "Submit to Judge", class = "btn-primary"),
      br(), br(),
      # --- Admin login UI ---
      tags$details(
        tags$summary("Admin login"),
        br(),
        passwordInput("admin_pw", "Admin password"),
        actionButton("admin_login", "Login"),
        uiOutput("admin_panel")   # if you already have extra admin controls
      ),
      width = 4
    ),
    mainPanel(
      h4("Your submission"),
      textAreaInput(
        inputId = "user_code",
        label = NULL,
        value = "",
        rows = 22,
        width = "100%",
        placeholder = "Write your R function here. The required function name depends on the selected problem."
      ),
      hr(),
      h4("Result"),
      htmlOutput("judge_summary"),
      tableOutput("wrong_table"),
      hr(),
      h4("Ranking (opt-in)"),
      p("If you want to appear in the ranking table, enter a display name,",
        "check the box below, and click 'Save my score' after running the judge."),
      textInput("rank_name", "Display name:", ""),
      checkboxInput(
        "rank_consent",
        "I want to add my latest result to the ranking table",
        value = FALSE
      ),
      actionButton("save_score", "Save my score"),
      br(), br(),
      DTOutput("score_table"),   # CHANGED (was tableOutput)
      downloadButton("download_scores", "Download scores")
    )
  )
)

# ----- Server -----

server <- function(input, output, session) {

  # Currently selected problem object
  selected_problem <- reactive({
    validate(
      need(length(problems) > 0, "No problems found in the 'problems' directory."),
      need(input$problem_id %in% names(problems), "Please select a problem.")
    )
    problems[[input$problem_id]]
  })

  # Persistent scoreboard (shared via scores.rds)
  score_data <- reactiveVal(load_scores())

  # Last judge result in this session (for ranking opt-in)
  last_result <- reactiveVal(NULL)

  # Show problem description
  output$problem_description <- renderText({
    p <- selected_problem()
    p$description
  })

  # Load template into textarea
  observeEvent(input$load_template, {
    p <- selected_problem()
    template <- p$template_code
    if (is.null(template)) template <- "# No template defined for this problem."
    updateTextAreaInput(session, "user_code", value = template)
  })

  # Helper: render plain text message in the result area
  render_plain_message <- function(msg) {
    output$judge_summary <- renderUI({
      HTML(
        paste0(
          "<pre>",
          htmltools::htmlEscape(msg),
          "</pre>"
        )
      )
    })
  }

  # Run judge
  observeEvent(input$run_judge, {
    p <- selected_problem()
    code <- input$user_code

    # Reset last_result on each run
    last_result(NULL)

    if (nchar(trimws(code)) == 0) {
      render_plain_message(
        "Submission is empty. Please implement the required function and try again."
      )
      output$wrong_table <- renderTable(NULL)
      return()
    }

    # Evaluate user code in an isolated environment
    user_env <- new.env(parent = baseenv())
    eval_res <- try(eval(parse(text = code), envir = user_env), silent = TRUE)

    if (inherits(eval_res, "try-error")) {
      render_plain_message(
        paste0(
          "Compilation error (RE)\n\n",
          as.character(eval_res)
        )
      )
      output$wrong_table <- renderTable(NULL)
      return()
    }

    fn_name <- p$function_name
    if (is.null(fn_name) || !nzchar(fn_name)) {
      render_plain_message(
        "This problem does not define a function_name. Please fix the problem definition."
      )
      output$wrong_table <- renderTable(NULL)
      return()
    }

    if (!exists(fn_name, envir = user_env, inherits = FALSE)) {
      render_plain_message(
        paste0(
          "Function '", fn_name, "' was not found in your submission.\n",
          "Please implement a function named ", fn_name, "() with the correct signature."
        )
      )
      output$wrong_table <- renderTable(NULL)
      return()
    }

    fun <- get(fn_name, envir = user_env)
    if (!is.function(fun)) {
      render_plain_message(
        paste0("Object '", fn_name, "' exists but is not a function. Please check your code.")
      )
      output$wrong_table <- renderTable(NULL)
      return()
    }

    # Run all tests for this problem
    res <- try(run_all_tests(fun, p), silent = TRUE)
    if (inherits(res, "try-error")) {
      render_plain_message(
        paste0("Internal judge error: ", as.character(res))
      )
      output$wrong_table <- renderTable(NULL)
      return()
    }

    # Store last_result for optional ranking (ensure timestamp is POSIXct)
    # ADDED: store source_code (the judged code)
    last_result(list(
      problem_id    = p$id,
      problem_title = p$title,
      status        = res$status,
      passed        = res$passed,
      total         = res$total,
      elapsed       = res$elapsed,
      timestamp     = as.POSIXct(Sys.time(), tz = "UTC"),
      source_code   = code
    ))

    # Prepare colored status label
    status_label <- switch(
      res$status,
      "AC"  = "AC (Accepted)",
      "WA"  = "WA (Wrong Answer)",
      "RE"  = "RE (Runtime Error)",
      "TLE" = "TLE (Time Limit Exceeded)",
      res$status
    )

    status_color <- if (res$status == "AC") {
      "#008000"  # green
    } else if (res$status == "TLE") {
      "#808080"  # gray
    } else {
      "#CC0000"  # red for WA / RE / others
    }

    status_html <- sprintf(
      '<span style="color:%s;font-weight:bold;">%s</span>',
      status_color, status_label
    )

    summary_html <- sprintf(
      '<pre>Status      : %s
Passed tests: %d / %d
Time        : %.4f seconds</pre>',
      status_html, res$passed, res$total, res$elapsed
    )

    if (length(res$wrong_cases) == 0) {
      detail_text <- "All tests passed. Great job!"
      wrong_df <- NULL
    } else {
      detail_text <- paste0(
        "Some tests failed or raised errors. ",
        "The table below shows a subset of failing cases."
      )

      # Helper: shorten long strings with "..."
      shorten_text <- function(txt, max_chars = 60) {
        txt <- gsub("\\s+", " ", txt)
        if (nchar(txt) > max_chars) {
          paste0(substr(txt, 1, max_chars - 3), "...")
        } else {
          txt
        }
      }

      # Format a single value for display in the table
      format_value <- function(v) {
        if (is.logical(v) && length(v) == 1) {
          return(ifelse(is.na(v), "NA", ifelse(v, "TRUE", "FALSE")))
        }
        if (is.character(v) && length(v) == 1) {
          return(shorten_text(v))
        }
        if (is.numeric(v) && length(v) == 1) {
          return(as.character(v))
        }
        if (is.atomic(v) && length(v) <= 5) {
          return(shorten_text(paste(v, collapse = ", ")))
        }
        shorten_text(paste0("<", class(v)[1], ", length = ", length(v), ">"))
      }

      # Format the argument list "input" for display
      summarize_input <- function(inp) {
        if (length(inp) == 0) return("")
        if (length(inp) == 1) {
          # Single argument: show only the value (typical for simple problems)
          return(format_value(inp[[1]]))
        }
        # Multiple arguments: show "name = value"
        parts <- mapply(
          function(name, value) {
            paste0(name, " = ", format_value(value))
          },
          name  = names(inp),
          value = inp,
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
        paste(parts, collapse = ", ")
      }

      wrong_df <- do.call(
        rbind,
        lapply(res$wrong_cases, function(x) {
          data.frame(
            test_id  = x$id,
            input    = summarize_input(x$input),
            expected = format_value(x$expected),
            got      = format_value(x$got),
            stringsAsFactors = FALSE
          )
        })
      )
    }

    # Render summary and detail as HTML (with colored status)
    output$judge_summary <- renderUI({
      safe_detail <- htmltools::htmlEscape(detail_text)
      safe_detail <- gsub("\n", "<br/>", safe_detail, fixed = TRUE)
      HTML(paste0(summary_html, "<br/>", safe_detail))
    })

    output$wrong_table <- renderTable({
      wrong_df
    })
  })

  # Save score to ranking table (opt-in), keeping only best per (problem_id, user_name)
  observeEvent(input$save_score, {
    lr <- last_result()

    if (is.null(lr)) {
      showNotification("No judge result to save. Please run the judge first.", type = "error")
      return()
    }

    if (!isTRUE(input$rank_consent)) {
      showNotification("Please check the consent box if you want to appear in the ranking.", type = "message")
      return()
    }

    name <- trimws(input$rank_name)
    if (name == "") {
      showNotification("Please enter a display name before saving your score.", type = "error")
      return()
    }

    # Reload scores from disk to reduce conflicts between sessions
    df <- load_scores()

    # New row for this submission
    new_row <- data.frame(
      problem_id    = lr$problem_id,
      problem_title = lr$problem_title,
      user_name     = name,
      status        = lr$status,
      passed        = lr$passed,
      total         = lr$total,
      elapsed       = lr$elapsed,
      timestamp     = as.POSIXct(lr$timestamp, tz = "UTC"),
      source_code   = lr$source_code,     # ADDED
      stringsAsFactors = FALSE
    )

    df <- rbind(df, new_row)

    # Define status ordering: better statuses come first
    status_order <- c("AC", "TLE", "WA", "RE")

    # Rank all rows so that "better" scores come first within each (problem_id, user_name)
    df$status_factor <- factor(df$status, levels = status_order, ordered = TRUE)

    df <- df[order(
      df$problem_id,
      df$user_name,
      df$status_factor,         # AC < TLE < WA < RE (ordered factor)
      -df$passed,               # more passed tests is better
      df$elapsed,               # smaller elapsed time is better
      df$timestamp              # earlier timestamp first
    ), ]

    df$status_factor <- NULL

    # Keep only the best row per (problem_id, user_name)
    keep_idx <- !duplicated(df[, c("problem_id", "user_name")])
    df <- df[keep_idx, ]

    # Save back to disk and update reactive value
    save_scores(df)
    score_data(df)

    showNotification(
      "Your score has been added to the ranking table (best result per name is kept).",
      type = "message"
    )
  })

  output$download_scores <- downloadHandler(
  filename = function() paste0("scores_", Sys.Date(), ".csv"),
  content = function(file) {
    df <- load_scores()

    # Format timestamp as text (and protect from Excel with leading apostrophe)
    if ("timestamp" %in% names(df)) {
      df$timestamp <- paste0("'", format(df$timestamp, "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
    }

    # Optional: control numeric formatting before converting to character
    if ("elapsed" %in% names(df)) df$elapsed <- sprintf("%.3f", df$elapsed)
    if ("passed"  %in% names(df)) df$passed  <- as.character(df$passed)
    if ("total"   %in% names(df)) df$total   <- as.character(df$total)

    # Convert all columns to character (text)
    df[] <- lapply(df, function(x) if (is.character(x)) x else as.character(x))

    write.csv(df, file = file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  },
  contentType = "text/csv"
)

  # --- View source modal handler (NEW) ---
  observeEvent(input$view_source_key, {
    req(input$view_source_key)

    df <- load_scores()
    if (!("source_code" %in% names(df))) df$source_code <- NA_character_

    # key = problem_id__user_name
    df$key <- paste(df$problem_id, df$user_name, sep = "__")
    row <- df[df$key == input$view_source_key, , drop = FALSE]

    if (nrow(row) == 0) {
      showNotification("Source not found (the ranking may have been updated).", type = "error")
      return()
    }

    code <- row$source_code[1]
    if (is.na(code) || !nzchar(code)) code <- "(No source code saved for this entry.)"

    showModal(modalDialog(
      title = paste0("Source code: ", row$user_name[1], " / ", row$problem_title[1]),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$pre(
        style = "max-height:60vh; overflow-y:auto; white-space:pre-wrap;",
        htmltools::htmlEscape(code)
      )
    ))
  })

  # Show ranking table for the selected problem (with colored status + Source button)
  output$score_table <- renderDT({
    df <- score_data()
    if (nrow(df) == 0) return(NULL)

    p <- selected_problem()
    df <- df[df$problem_id == p$id, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)

    if (!("source_code" %in% names(df))) df$source_code <- NA_character_

    status_order <- c("AC", "TLE", "WA", "RE")
    df$status_factor <- factor(df$status, levels = status_order, ordered = TRUE)

    df <- df[order(df$status_factor,
                   -df$passed,
                   df$elapsed,
                   df$timestamp), ]

    df$status_factor <- NULL

    # Color status column
    status_color <- ifelse(
      df$status == "AC", "#008000",
      ifelse(df$status == "TLE", "#808080", "#CC0000")
    )
    df$status <- sprintf(
      '<span style="color:%s;font-weight:bold;">%s</span>',
      status_color, df$status
    )

    # Format timestamp nicely for display
    df$timestamp <- format(df$timestamp, "%Y-%m-%d %H:%M:%S")

    # Add "Source" button column
    df$key <- paste(df$problem_id, df$user_name, sep = "__")
    df$Source <- sprintf(
      '<button class="btn btn-sm btn-info" onclick="Shiny.setInputValue(\'view_source_key\', \'%s\', {priority: \'event\'})">View</button>',
      df$key
    )

    show_df <- df[, c("user_name", "status", "passed", "total", "elapsed", "timestamp", "Source")]

    dt <- datatable(
      show_df,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 20,
        autoWidth = TRUE
      )
    )

    # Round elapsed to 3 decimals (and you can do passed/total as integers too)
    dt <- formatRound(dt, columns = "elapsed", digits = 3)

    dt
  })

  # --- Admin state ---
  is_admin <- reactiveVal(FALSE)

  # Handle admin login
  observeEvent(input$admin_login, {
    if (!nzchar(admin_password)) {
      showNotification(
        "Admin password is not configured on the server (SCORES_ADMIN_PASSWORD).",
        type = "error"
      )
      return()
    }

    if (identical(input$admin_pw, admin_password)) {
      is_admin(TRUE)
      showNotification("Admin mode enabled.", type = "message")
    } else {
      showNotification("Incorrect password.", type = "error")
    }
  })

  # Admin-only panel (shown after successful login)
  output$admin_panel <- renderUI({
    req(is_admin())

    tagList(
      tags$hr(),
      h4("Admin: import scores"),
      fileInput(
        "scores_file",
        "Select scores.csv",
        accept = c(".csv", "text/csv")
      ),
      actionButton("import_scores", "Import scores", class = "btn-danger"),
      helpText("Use a scores.csv file previously downloaded from this app.")
    )
  })

  # Handle CSV import
  observeEvent(input$import_scores, {
    req(is_admin())
    req(input$scores_file)

    df <- tryCatch(
      readr::read_csv(input$scores_file$datapath, show_col_types = FALSE),
      error = function(e) {
        showNotification(
          paste("Failed to read CSV:", e$message),
          type = "error"
        )
        return(NULL)
      }
    )
    req(!is.null(df))

    # Minimum required columns
    required_cols <- c("problem_id","problem_title","user_name", "status", "passed", "total", "elapsed", "timestamp")
    if (!all(required_cols %in% names(df))) {
      showNotification(
        paste(
          "CSV columns do not match expected columns. Expected at least:",
          paste(required_cols, collapse = ", ")
        ),
        type = "error"
      )
      return()
    }

    # Optional source_code column
    if (!("source_code" %in% names(df))) df$source_code <- NA_character_

    # Robust timestamp conversion
    df$timestamp <- parse_timestamp_utc(df$timestamp)
    if (any(is.na(df$timestamp))) {
      showNotification(
        "Some timestamps could not be parsed and were set to NA. Please check your CSV timestamp format.",
        type = "warning"
      )
    }
    # Overwrite current scores
    save_scores(df)
    score_data(df)
    showNotification("Scores have been imported.", type = "message")
  })
}

shinyApp(ui = ui, server = server)
