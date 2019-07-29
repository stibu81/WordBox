# Define the UI #####
ui <- fluidPage(

    titlePanel("WordBox Vokabeltrainer"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            fileInput("wordlist_file",
                      "W\u00f6rterliste laden",
                      width = "300px",
                      accept = ".csv",
                      buttonLabel = "Suche...",
                      placeholder = "keine W\u00f6rterliste geladen"),
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
                            wl = NULL,
                            quiz = NULL,
                            question = 0,
                            i_exercise = 0,
                            show_answer = FALSE,
                            n_correct = 0,
                            n_wrong = 0)

    # load a file
    observe({
        file <- input$wordlist_file
        if (!is.null(file)) {
            state$wl <- read_wordlist(file$datapath)
            langs <- get_languages(state$wl)
            choices <- magrittr::set_names(paste0("direction", 1:2),
                                           paste0(langs, " > ", rev(langs)))
            updateSelectInput(session, "direction", choices = choices)
        }
    })

    output$current_box <- renderText({
        if (state$running) state$question$box
    })

    output$question <- renderText({
        if (state$running) state$question$question
    })

    output$current_group <- renderText({
        if (state$running) state$question$group
    })

    output$n_correct <- renderText({
        if (state$running) state$n_correct
    })

    output$n_wrong <- renderText({
        if (state$running) state$n_wrong
    })


    # start the exercise
    observeEvent(input$run,
        if (!state$running) {
            state$running <- TRUE
            cat("running exercise with",
                input$direction, "and mode", input$mode)
            direction <- gsub("direction", "", input$direction) %>%
                            as.numeric()
            state$quiz <- prepare_quiz(state$wl, direction)
            state$i_exercise <- state$i_exercise + 1
        }
    )

    # create the UI when the exercise is started
    output$exerciseUI <- renderUI({
        if (!state$running) return(NULL)
        if (input$mode == "written") {
            tagList(
                textInput("solution_in", "Übersetzung"),
                actionButton("check_written", "Prüfen"),
                br(), br(),
                strong("Lösung"),
                textOutput("solution"),
                br(),
                actionButton("gonext", "Weiter")
            )
        } else {
            tagList(
                actionButton("check", "Prüfen"),
                br(), br(),
                strong("Lösung"),
                textOutput("solution"),
                br(),
                actionButton("correct", "Richtig"),
                actionButton("wrong", "Falsch")
            )
        }
    })

    # initialise a new exercise
    observeEvent(state$i_exercise, {
        if (state$running) {
            state$question <- draw_question(state$quiz, state$wl)
        }
    })

    # button check: show the solution
    observeEvent(input$check,
        if (state$running) state$show_answer <- TRUE)
    output$solution <- renderText({
        if (state$show_answer) {
            state$question$answer
        } else {
            " "
        }
    })

    # buttons next, correct, wrong
    observeEvent(input$gonext, {
        state$show_answer <- FALSE
        state$i_exercise <- state$i_exercise + 1
    })
    observeEvent(input$correct, {
        state$show_answer <- FALSE
        state$n_correct <- state$n_correct + 1
        state$i_exercise <- state$i_exercise + 1
    })
    observeEvent(input$wrong, {
        state$show_answer <- FALSE
        state$n_wrong <- state$n_wrong + 1
        state$i_exercise <- state$i_exercise + 1
    })



}

#' Run the application
#'
#' @export

run_wordbox <- function() {
    shiny::shinyApp(ui = ui, server = server)
}
