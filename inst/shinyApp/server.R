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
                            n_wrong = 0,
                            dot_colour = "white")

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
            if (is.null(state$question)) {
                showModal(
                    modalDialog("Du hast alle Fragen beantwortet!",
                                titel = "Information")
                )
                stopApp()
            }
        }
    })

    # button check: show the solution in oral mode,
    # check user input and show the solution in written mode
    # if the button is clicked a second time, the check should not
    # be redone! This is achieved by not running the check, if the
    # answer is shown.
    observeEvent(input$check, {
        if (state$running && !state$show_answer) {
            if (input$mode == "written") {
                if (trimws(input$solution_in) == state$question$answer) {
                    # mark the word in the wordlist and remove from quiz
                    state$wl <- mark_word(state$question,
                                          state$quiz,
                                          state$wl,
                                          success = TRUE)
                    state$quiz <- state$quiz[-state$question$i_quiz, ]
                    state$dot_colour <- "green"
                    state$n_correct <- state$n_correct + 1
                } else {
                    # mark the word in the wordlist, leave in quiz
                    state$wl <- mark_word(state$question,
                                          state$quiz,
                                          state$wl,
                                          success = FALSE)
                    state$dot_colour <- "red"
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
        state$dot_colour <- "green"
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
        state$dot_colour <- "red"
        state$i_exercise <- state$i_exercise + 1
    })

    # text outputs
    output$current_box <- renderText(state$question$box)
    output$question <- renderText(state$question$question)
    output$current_group <- renderText(state$question$group)
    output$n_words <- renderText({
        if (is.null(state$quiz)) 0 else nrow(state$quiz)
    })
    output$n_correct <- renderText(state$n_correct)
    output$n_wrong <- renderText(state$n_wrong)
    output$solution <- renderText({
        if (state$show_answer) state$question$answer
    })

    # render the coloured dot
    output$dot <- renderPlot({
        ggplot2::ggplot() +
            ggplot2::annotate("polygon",
                              x = c(0, 2*pi), y =  c(1, 1),
                              fill = rep(state$dot_colour, 2)) +
            ggplot2::coord_polar() +
            ggplot2::scale_y_continuous(limits = c(0, 1)) +
            ggplot2::theme_void()
    })

}
