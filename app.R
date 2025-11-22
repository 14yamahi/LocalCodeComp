# app.R
# Generic Shiny Online Judge for R coding contests
# - Problems are loaded from ./problems/<problem_id>/problem.R
# - Each problem defines:
#     problem <- list(
#       id, title, description, function_name,
#       template_code, time_limit_sec, make_tests, (optional) compare
#     )

library(shiny)

# ----- Scoreboard helpers (opt-in ranking) -----

scores_file <- "scores.rds"

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
    stringsAsFactors = FALSE
  )
}

load_scores <- function() {
  if (!file.exists(scores_file)) return(empty_scores())
  out <- try(readRDS(scores_file), silent = TRUE)
  if (inherits(out, "try-error")) empty_scores() else out
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
      verbatimTextOutput("judge_result"),
      tableOutput("wrong_table"),
      hr(),
      h4("Ranking (opt-in)"),
      p("If you want to appear in the ranking table, enter a display name,",
        "check the box below, and click 'Save my score' after running the judge."),
      textInput("rank_name", "Display name:", ""),
      checkboxInput("rank_consent",
                    "I want to add my latest result to the ranking table",
                    value = FALSE),
      actionButton("save_score", "Save my score"),
      br(), br(),
      tableOutput("score_table")
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

  # Run judge
  observeEvent(input$run_judge, {
    p <- selected_problem()
    code <- input$user_code

    # Reset last_result on each run
    last_result(NULL)

    if (nchar(trimws(code)) == 0) {
      output$judge_result <- renderText({
        "Submission is empty. Please implement the required function and try again."
      })
      output$wrong_table <- renderTable(NULL)
      return()
    }

    # Evaluate user code in an isolated environment
    user_env <- new.env(parent = baseenv())
    eval_res <- try(eval(parse(text = code), envir = user_env), silent = TRUE)

    if (inherits(eval_res, "try-error")) {
      output$judge_result <- renderText({
        paste0(
          "Compilation error (RE)\n\n",
          as.character(eval_res)
        )
      })
      output$wrong_table <- renderTable(NULL)
      return()
    }

    fn_name <- p$function_name
    if (is.null(fn_name) || !nzchar(fn_name)) {
      output$judge_result <- renderText({
        "This problem does not define a function_name. Please fix the problem definition."
      })
      output$wrong_table <- renderTable(NULL)
      return()
    }

    if (!exists(fn_name, envir = user_env, inherits = FALSE)) {
      output$judge_result <- renderText({
        paste0(
          "Function '", fn_name, "' was not found in your submission.\n",
          "Please implement a function named ", fn_name, "() with the correct signature."
        )
      })
      output$wrong_table <- renderTable(NULL)
      return()
    }

    fun <- get(fn_name, envir = user_env)
    if (!is.function(fun)) {
      output$judge_result <- renderText({
        paste0("Object '", fn_name, "' exists but is not a function. Please check your code.")
      })
      output$wrong_table <- renderTable(NULL)
      return()
    }

    # Run all tests for this problem
    res <- try(run_all_tests(fun, p), silent = TRUE)
    if (inherits(res, "try-error")) {
      output$judge_result <- renderText({
        paste0("Internal judge error: ", as.character(res))
      })
      output$wrong_table <- renderTable(NULL)
      return()
    }

    # Store last_result for optional ranking
    last_result(list(
      problem_id    = p$id,
      problem_title = p$title,
      status        = res$status,
      passed        = res$passed,
      total         = res$total,
      elapsed       = res$elapsed,
      timestamp     = Sys.time()
    ))

    # Prepare summary text
    status_label <- switch(
      res$status,
      "AC"  = "AC (Accepted)",
      "WA"  = "WA (Wrong Answer)",
      "RE"  = "RE (Runtime Error)",
      "TLE" = "TLE (Time Limit Exceeded)",
      res$status
    )

    summary_text <- paste0(
      "Status      : ", status_label, "\n",
      "Passed tests: ", res$passed, " / ", res$total, "\n",
      sprintf("Time        : %.4f seconds\n", res$elapsed)
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

    output$judge_result <- renderText({
      paste0(summary_text, "\n", detail_text)
    })

    output$wrong_table <- renderTable({
      wrong_df
    })
  })

  # Save score to ranking table (opt-in)
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

    # Reload scores to reduce conflicts between sessions
    df <- load_scores()
    new_row <- data.frame(
      problem_id    = lr$problem_id,
      problem_title = lr$problem_title,
      user_name     = name,
      status        = lr$status,
      passed        = lr$passed,
      total         = lr$total,
      elapsed       = lr$elapsed,
      timestamp     = lr$timestamp,
      stringsAsFactors = FALSE
    )
    df <- rbind(df, new_row)
    save_scores(df)
    score_data(df)

    showNotification("Your score has been added to the ranking table.", type = "message")
  })

  # Show ranking table for the selected problem
  output$score_table <- renderTable({
    df <- score_data()
    if (nrow(df) == 0) return(NULL)

    p <- selected_problem()
    df <- df[df$problem_id == p$id, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)

    status_order <- c("AC", "TLE", "WA", "RE")
    df$status_factor <- factor(df$status, levels = status_order, ordered = TRUE)

    df <- df[order(df$status_factor,
                   -df$passed,
                   df$elapsed,
                   df$timestamp), ]

    df$status_factor <- NULL

    df[, c("user_name", "status", "passed", "total", "elapsed", "timestamp")]
  }, digits = 3)
}

shinyApp(ui = ui, server = server)
