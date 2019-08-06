# Define the UI #####
ui <- fluidPage(

    titlePanel("WordBox Vokabeltrainer"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            selectInput("wordlist_file",
                       "W\u00f6rterliste laden",
                       "kein Verzeichnis ausgew\u00e4hlt",
                       width = "300px"),
            actionButton("load",
                         "Laden",
                         width = "100px"),
            br(), br(),
            selectInput("direction",
                        "Richtung ausw\u00e4hlen",
                        "keine W\u00f6rterliste geladen",
                        width = "300px"),
            selectInput("mode",
                        "Modus ausw\u00e4hlen",
                        c("schriftlich" = "written",
                          "m\u00fcndlich" = "oral"),
                        selected = "written",
                        width = "300px"),
            actionButton("run",
                         "Start",
                         width = "100px")
        ),

        # main panel
        mainPanel(
             "Anzahl richtige Antworten: ",
            div(style = "display:inline-block", textOutput("n_correct")),
            br(), "Anzahl falsche Antworten: ",
            div(style = "display:inline-block", textOutput("n_wrong")),
            br(), br(),
            "Fach: ",
            div(style = "display:inline-block", textOutput("current_box")),
            br(), "Gruppe: ",
            div(style = "display:inline-block", textOutput("current_group")),
            br(), br(),
            strong("Aufgabe"),
            textOutput("question"),
            br(),
            uiOutput("exerciseUI")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # create an object to track the state of the application
    state <- reactiveValues(running = FALSE,
                            wl_file = NULL,
                            mode = NULL,
                            direction = NULL,
                            wl = NULL,
                            quiz = NULL,
                            question = NULL,
                            i_exercise = 0,
                            show_answer = FALSE,
                            n_correct = 0,
                            n_wrong = 0)

    # read in file names in the directory
    updateSelectInput(session, "wordlist_file",
                      choices = list.files(getOption("wordbox_dir"),
                                           ".csv$"))

    # load a file
    observeEvent(input$load,
        if (!state$running) {
            state$wl_file <- file.path(getOption("wordbox_dir"),
                                       input$wordlist_file)
            if (file.exists(state$wl_file)) {
                state$wl <- read_wordlist(state$wl_file)
                langs <- get_languages(state$wl)
                choices <- magrittr::set_names(paste0("direction", 1:2),
                                               paste0(langs, " > ", rev(langs)))
                updateSelectInput(session, "direction", choices = choices)
            }
        }
    )

    # start the quiz
    observeEvent(input$run,
        if (!is.null(state$wl) && !state$running) {
            state$running <- TRUE
            cat("running exercise with",
                input$direction, "and mode", input$mode, "\n")
            state$direction <- gsub("direction", "", input$direction) %>%
                                as.numeric()
            state$mode <- input$mode
            state$quiz <- prepare_quiz(state$wl, state$direction)
            state$i_exercise <- state$i_exercise + 1
        }
    )

    # create the UI when the quiz is started
    output$exerciseUI <- renderUI({
        if (!state$running) return(NULL)
        if (state$mode == "written") {
            tagList(
                textInput("solution_in", "\u00dcbersetzung"),
                actionButton("check", "Pr\u00fcfen"),
                br(), br(),
                strong("L\u00f6sung"),
                textOutput("solution"),
                br(),
                actionButton("gonext", "Weiter")
            )
        } else {
            tagList(
                actionButton("check", "Pr\u00fcfen"),
                br(), br(),
                strong("L\u00f6sung"),
                textOutput("solution"),
                br(),
                actionButton("correct", "Richtig"),
                actionButton("wrong", "Falsch")
            )
        }
    })

    # initialise a new quiz whenever the counter i_exercise
    # is incremented
    observeEvent(state$i_exercise, {
        if (state$running) {
            state$question <- draw_question(state$quiz, state$wl)
            write_wordlist(state$wl, state$wl_file, TRUE)
        }
    })

    # button check: show the solution in oral mode,
    # check user input and show the solution in written mode
    observeEvent(input$check, {
        if (state$running) {
            if (input$mode == "written") {
                if (trimws(input$solution_in) == state$question$answer) {
                    # mark the word in the wordlist and remove from quiz
                    state$wl <- mark_word(state$question,
                                          state$quiz,
                                          state$wl,
                                          success = TRUE)
                    state$quiz <- state$quiz[-state$question$i_quiz, ]
                    state$n_correct <- state$n_correct + 1
                } else {
                    # mark the word in the wordlist, leave in quiz
                    state$wl <- mark_word(state$question,
                                          state$quiz,
                                          state$wl,
                                          success = FALSE)
                    state$n_wrong <- state$n_wrong + 1
                }
            }
            state$show_answer <- TRUE
        }
    })

    # buttons next, correct, wrong
    observeEvent(input$gonext, {
        if (state$show_answer) {
            state$show_answer <- FALSE
            state$i_exercise <- state$i_exercise + 1
            updateTextInput(session, "solution_in", value = "")
        }
    })
    observeEvent(input$correct, {
        state$show_answer <- FALSE
        # mark the word in the wordlist and remove from quiz
        state$wl <- mark_word(state$question,
                                          state$quiz,
                                          state$wl,
                                          success = TRUE)
        state$quiz <- state$quiz[-state$question$i_quiz, ]
        state$n_correct <- state$n_correct + 1
        state$i_exercise <- state$i_exercise + 1
    })
    observeEvent(input$wrong, {
        state$show_answer <- FALSE
        # mark the word in the wordlist, leave in quiz
        state$wl <- mark_word(state$question,
                                          state$quiz,
                                          state$wl,
                                          success = FALSE)
        state$n_wrong <- state$n_wrong + 1
        state$i_exercise <- state$i_exercise + 1
    })

    # text outputs
    output$current_box <- renderText(state$question$box)
    output$question <- renderText(state$question$question)
    output$current_group <- renderText(state$question$group)
    output$n_correct <- renderText(state$n_correct)
    output$n_wrong <- renderText(state$n_wrong)
    output$solution <- renderText({
        if (state$show_answer) state$question$answer
    })
}

#' Run the application
#'
#' @param dir character indicating a directory where wordlist
#'  files (csv) are stored. All the csv files in that directory
#'  will be available to be loaded in the application.
#' @param launch.browser logical, if \code{TRUE}, the application
#'  is opened in the system's default browser.
#'
#' @export

run_wordbox <- function(dir = NULL, launch.browser = FALSE) {
    if (is.null(dir) || !dir.exists(dir)) {
        stop("an existing directory must be provided.")
    }
    options(wordbox_dir = dir)
    opts <- if (launch.browser) list(launch.browser = TRUE) else list()
    shiny::shinyApp(ui = ui, server = server,
                    options = opts)
}
