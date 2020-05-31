library(magrittr)
library(WordBox)

server <- function(input, output, session) {

  # create an object to track the state of the application
  state <- WordBox:::get_initial_state()

  # read in file names in the directory #####
  updateSelectInput(session, "wordlist_file",
                    choices = getOption("wordbox_dir") %>%
                                list.files(".csv$") %>%
                                gsub(".csv$", "", .))

  # load a file #####
  observeEvent(
    input$load,
    {
      # on loading a file, stop the running quiz
      state$running <- FALSE
      state$show_answer <- FALSE
      state$question <- NULL

      state$wl_file <- file.path(getOption("wordbox_dir"),
                                 paste0(input$wordlist_file,
                                        ".csv"))
      if (file.exists(state$wl_file)) {
        state$wl <- WordBox:::prepare_quiz_gui(session, state)
      }
    }
  )

  # start the quiz #####
  observeEvent(
    input$run,
    if (!is.null(state$wl) && !state$running) {
      state$running <- TRUE
      direction <- gsub("direction", "", input$direction) %>%
        as.numeric()
      state$mode <- input$mode
      state$n_words <- input$n_words
      state$groups <- input$groups
      state$quiz <- prepare_quiz(state$wl, direction,
                                 quiz_type = input$quiztype,
                                 groups = state$groups,
                                 n_words = state$n_words)
      state$i_exercise <- state$i_exercise + 1
      state$n_correct <- 0
      state$n_wrong <- 0
      shinyjs::disable("run")
      message("running exercise from file ", state$wl_file,
              " with the following settings:",
              "\nDirection: ", direction,
              "\nMode: ", state$mode,
              "\nQuiztype: ", get_quiz_type(state$quiz),
              "\n# of words: ", state$n_words,
              "\nGroups :", state$groups, "\n")
    }
  )

  # dynamic UI for the quiz ######
  output$exerciseUI <- renderUI(WordBox:::create_quiz_ui(state))

  # draw a new question #####
  # this is triggered by incrementing i_exercise
  observeEvent(state$i_exercise, {
    if (state$running) {
      state$question <- draw_question(state$quiz, state$wl, state$question)
      # save wordlist only if not in training mode
      if (get_quiz_type(state$quiz) != "training")
        write_wordlist(state$wl, state$wl_file, TRUE)
      if (is.null(state$question)) {
        showModal(
          modalDialog("Du hast alle Fragen beantwortet!",
                      titel = "Information")
        )
        # reset to original state
        state$wl <- NULL
        state$running <- FALSE
        updateRadioButtons(session,
                           "direction",
                            choices = c(">" = "direction1",
                                        "<" = "direction2"))
        updateSelectInput(session, "group",
                          choices = NULL)
      }
    }
  })

  # click on actionButton check ######
  # show the solution in oral mode,
  # check user input and show the solution in written mode
  # if the button is clicked a second time, the check should not
  # be redone! This is achieved by not running the check, if the
  # answer is shown.
  observeEvent(input$check, {
    if (state$running && !state$show_answer) {
      if (input$mode == "written") {
        success <- correct_answer(input$solution_in, state$question,
              rm_trailing_chars = getOption("wordbox_rm_trailing_chars"))
        # mark the word in the wordlist and the quiz
        state$wl <- mark_word(state$question,
                              state$quiz,
                              state$wl,
                              success)
        state$quiz <- update_quiz(state$question,
                                state$quiz,
                                state$wl,
                                success)
        state$dot_colour <- c("red", "green")[success + 1]
        state$n_correct <- state$n_correct + success
        state$n_wrong <- state$n_wrong + !success
        shinyjs::enable("gonext")
      } else {
        shinyjs::enable("correct")
        shinyjs::enable("wrong")
      }
      state$show_answer <- TRUE
    }
  })

  # click on actionButton gonext #####
  observeEvent(input$gonext, {
    if (state$show_answer) {
      state$show_answer <- FALSE
      state$i_exercise <- state$i_exercise + 1
      updateTextInput(session, "solution_in", value = "")
      shinyjs::disable("gonext")
    }
  })

  # click on actionButton correct & wrong #####
  # both buttons only work, if the answer is shown
  observeEvent(input$correct, {
    if (state$show_answer) {
      state$show_answer <- FALSE
      # mark the word in the wordlist and remove from quiz
      state$wl <- mark_word(state$question,
                            state$quiz,
                            state$wl,
                            success = TRUE)
      state$quiz <- update_quiz(state$question,
                              state$quiz,
                              state$wl,
                              success = TRUE)
      state$dot_colour <- "green"
      state$n_correct <- state$n_correct + 1
      state$i_exercise <- state$i_exercise + 1
      shinyjs::disable("correct")
      shinyjs::disable("wrong")
    }
  })
  observeEvent(input$wrong, {
    if (state$show_answer) {
      state$show_answer <- FALSE
      # mark the word in the wordlist, leave the quiz unchanged
      state$wl <- mark_word(state$question,
                            state$quiz,
                            state$wl,
                            success = FALSE)
      state$n_wrong <- state$n_wrong + 1
      state$dot_colour <- "red"
      state$i_exercise <- state$i_exercise + 1
      shinyjs::disable("correct")
      shinyjs::disable("wrong")
    }
  })

  # register keys for keyboard input
  WordBox:::register_keys(state)

  # text outputs #####
  output$current_box <- renderText(state$question$box)
  output$question <- renderText(state$question$question)
  output$current_group <- renderText(state$question$group)
  output$n_words <- renderText({nrow(state$quiz)})
  output$n_correct <- renderText(state$n_correct)
  output$n_wrong <- renderText({
    if (getOption("wordbox_show_errors")) state$n_wrong
  })
  output$solution <- renderText({
    if (state$show_answer) {
      paste(unique(state$question$answers), collapse = "; ")
    }
  })

  # render the coloured dot #####
  output$dot <- renderPlot({
    ggplot2::ggplot() +
      ggplot2::annotate("polygon",
                        x = c(0, 2*pi), y =  c(1, 1),
                        fill = rep(state$dot_colour, 2)) +
      ggplot2::coord_polar() +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::theme_void()
  })

  # stop app when session ends
  session$onSessionEnded(function() {
        stopApp()
    })

}
