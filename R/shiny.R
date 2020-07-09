#' Run the application
#'
#' @param dir character indicating a directory where wordlist
#'  files (csv) are stored. All the csv files in that directory
#'  will be available to be loaded in the application.
#' @param launch.browser logical, if \code{TRUE}, the application
#'  is opened in the system's default browser, if \code{FALSE},
#'  no browser is started. If the argument is omitted, the value
#'  according to the option \code{shiny.launch.browser} is used,
#'  which in RStudio opens the internal shiny viewer.
#' @param config_file character giving the full path to the
#'  configuration file. If omitted, the default configuration is
#'  used. See \code{\link{read_config}} for details
#'  on the config file.
#' @param show_errors should the UI show the number of errors
#'  that were made during a quiz.
#' @param n_words_default integer indicating the default for the
#'  number of words per quiz. This value must lie between 5
#'  and 95.
#' @inheritParams correct_answer
#'
#' @export

run_wordbox <- function(dir = NULL, launch.browser = NULL,
                        config_file = NULL,
                        show_errors = TRUE,
                        n_words_default = 30,
                        rm_trailing_chars = "") {

    if (is.null(dir) || !dir.exists(dir)) {
        stop("an existing directory must be provided.")
    }
    options(wordbox_dir = dir)
    appDir <- system.file("shinyApp", package = "WordBox")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `WordBox`.",
           call. = FALSE)
    }

    # check: n_words_default must be between 5 and 95
    if (!dplyr::between(n_words_default, 5, 95)) {
      stop("n_words_default must lie between 5 and 95")
    }
    n_words_default <- floor(n_words_default)

    options(wordbox_cfg_file = config_file,
            wordbox_show_errors = show_errors,
            wordbox_rm_trailing_chars = rm_trailing_chars,
            wordbox_n_words_default = n_words_default)

    if (is.null(launch.browser)) {
        launch.browser <- getOption("shiny.launch.browser", interactive())
    }

    shiny::runApp(appDir, display.mode = "normal",
                  launch.browser = launch.browser)
}


# Return initial state for the app
get_initial_state <- function() {
  shiny::reactiveValues(
    running = FALSE,
    wl_file = NULL,
    mode = NULL,
    groups = NULL,
    n_words = NULL,
    wl = NULL,
    quiz = NULL,
    question = NULL,
    i_exercise = 0,
    show_answer = FALSE,
    n_correct = NULL,
    n_wrong = NULL,
    icon = ""
  )
}


prepare_quiz_gui <- function(session, state) {

  cfg_file <- getOption("wordbox_cfg_file")
  message("reading wordlist file ", state$wl_file,
          "\nwith ", if (is.null(cfg_file)) "default",
          " config file ", cfg_file, "\n")
  wl <- read_wordlist(state$wl_file, cfg_file)

  langs <- get_languages(wl)
  choices <- magrittr::set_names(paste0("direction", 1:2),
                                 paste0(langs, " > ", rev(langs)))
  shiny::updateRadioButtons(session, "direction", choices = choices)
  shiny::updateSelectInput(session, "groups",
                           choices = get_groups(wl))
  shinyjs::enable("run")

  wl
}


# register keys to press buttons through keyboard input
register_keys <- function(state) {

  # if Enter is pressed while the text input is active
  # either check or gonext are clicked
  shinyjs::onevent("keyup", "solution_in", function(e) {
        if (e$keyCode == 13) {
          if (!state$show_answer) {
            shinyjs::click("check")
          } else {
            shinyjs::click("gonext")
          }
        }
      })

  # if the button check is selected, the buttons
  # correct and wrong can be clicked by pressing
  # r (correct) and f (wrong)
  shinyjs::onevent("keyup", "check", function(e) {
      if (state$mode == "oral" && state$show_answer) {
        if (e$key == "r") {
          shinyjs::click("correct")
        }
        if (e$key == "f") {
          shinyjs::click("wrong")
        }
      }
    })
}


create_quiz_ui <- function(state) {

  if (!state$running) return(NULL)

  if (state$mode == "written") {
    shiny::tagList(
        shiny::tags$p(shiny::tags$b("Antwort")),
        create_text_input_row("", "", state$icon),
        shiny::actionButton("check", "Pr\u00fcfen"),
        shiny::br(), shiny::br(),
        shiny::strong("L\u00f6sung"),
        shiny::textOutput("solution"),
        shiny::br(),
        shiny::actionButton("gonext", "Weiter")
      )
    } else {
      shiny::tagList(
        shiny::actionButton("check", "Pr\u00fcfen"),
        shiny::br(), shiny::br(),
        shiny::strong("L\u00f6sung"),
        shiny::textOutput("solution"),
        shiny::br(),
        shiny::actionButton("correct", "Richtig",
                            icon = shiny::icon("check-circle")),
        shiny::actionButton("wrong", "Falsch",
                            icon = shiny::icon("times-circle"))
      )
    }
}


# Create a row with one or two text inputs
create_text_input_row <- function(idx, pref = "", state = "") {

  # length of idx determines the number of text boxes.
  n <- length(idx)
  if (!n %in% 1:2) stop("idx must have length 1 or 2")

  # pref and state must be same length as ids
  if (length(pref) < n) pref <- rep(pref, 2)
  if (length(state) < n) state <- rep(state, 2)

  # helper functions for the rows
  fpref <- function(p) {
    if (p == "") {
      NULL
    } else {
      shiny::column(1, shiny::tags$p(p))
    }
  }
  ftext <- function(i, p) {
    width <- if (p == "") 5 else 4
    id <- paste0("solution_in", i)
    shiny::column(
      width,
      shiny::textInput(id, width = "100%", label = NULL)
    )
  }
  ficon <- function(s) {
    icon_name <- dplyr::case_when(
      s == "ok" ~ "check-circle",
      s == "nok" ~ "times-circle",
      s == "ask" ~ "question-circle",
      s == "retry" ~ "exclamation-triangle",
      TRUE ~ ""
    )
    if (icon_name == "") {
      NULL
    } else {
      shiny::column(1, shiny::icon(icon_name))
    }
  }

  # create the fluid row
  shiny::fluidRow(
    fpref(pref[1]),
    ftext(idx[1], pref[1]),
    ficon(state[1]),
    if (n == 2) fpref(pref[2]),
    if (n == 2) ftext(idx[2], pref[2]),
    if (n == 2) ficon(state[2]),
  )
}
