library(magrittr)
library(WordBox)

server <- function(input, output, session) {

  # create an object to track the state of the application
  state <- WordBox:::get_initial_state()

  # create a variable that stores the name of the object
  # where focus should go after all observers have finished
  # executing.
  refocus_to <- NULL

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
                                 n_words = state$n_words,
                                 core_only = input$core_only,
                                 exam_only = input$exam_only,
                                 log_file = getOption("wordbox_log_file"))
      WordBox:::write_log(state$quiz, "mode:", state$mode)
      state$i_exercise <- state$i_exercise + 1
      state$n_correct <- 0
      state$n_wrong <- 0
      # changing the icon triggers redrawing of the quiz UI
      state$icon <- ""
      shinyjs::disable("run")
      message("running exercise from file ", state$wl_file,
              " with the following settings:",
              "\nDirection: ", direction,
              "\nMode: ", state$mode,
              "\nQuiztype: ", get_quiz_type(state$quiz),
              "\n# of words: ", state$n_words,
              "\ncore only: ", input$core_only,
              "\nexam only: ", input$exam_only,
              "\nGroups :", state$groups, "\n")
    }
  )

  # dynamic UI for the quiz ######
  output$exerciseUI <- renderUI({
    if (state$running) {
      refocus_to <<- if (state$mode == "written") "solution_in" else "check"
      # redraw the UI whenever the icons change. This works, because
      # the icon changes to "ask", when a new question is asked and
      # to "ok", "nok", or "retry" when an answer is evaluated.
      state$icon
      # use isolate to avoid an infinite loop
      isolate({
        ui <- WordBox:::create_quiz_ui(state, session, input)
        # onyl set reset_ui to FALSE if an interface was successfully created
        # this avoids keeping the contents if a new quiz is started in
        # the same session
        if (!is.null(ui)) state$reset_ui <- FALSE
      })
      ui
    }
  })

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
                      title = "Information",
                      footer = modalButton("OK"),
                      easyClose = TRUE)
        )
        # reset to original state
        state$wl <- NULL
        state$running <- FALSE
        state$reset_ui <- TRUE
        shinyWidgets::updateAwesomeRadio(session,
                           "direction",
                            choices = c(">" = "direction1",
                                        "<" = "direction2"))
        updateSelectInput(session, "group",
                          choices = NULL)
      } else {
        # prepare the icons
        n_icon <- list(verb = 7, single = 1)
        state$icon <- rep("ask", n_icon[[state$question$type]])
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
    # flag to decide, whether the word should be marked or not
    # this is used to allow retries, where the word is NOT marked
    mark_word <- FALSE
    if (state$running && !state$show_answer) {
      if (input$mode == "written") {
        if (state$question$type == "single") {
          success <- correct_answer(
            input$solution_in,
            state$question,
            rm_trailing_chars = getOption("wordbox_rm_trailing_chars"))
          mark_word <- TRUE
        } else {
          answers <- vapply(paste0("solution_in", c("", 1:6)),
                            function(n) input[[n]],
                            character(1))
          success <- correct_answer(
            answers,
            state$question,
            rm_trailing_chars = getOption("wordbox_rm_trailing_chars")
          )
          # if there are one or two errors, allow for retry
          # if this is not already a retry
          if (sum(!success) %in% 1:2 && !state$retry) {
            state$icon <- c("retry", "ok")[success + 1]
            state$retry <- TRUE
            WordBox:::write_log(state$quiz, "allow retry")
          } else {
            mark_word <- TRUE
          }
        }
        # mark the word in the wordlist and the quiz
        if (mark_word) {
          state$wl <- mark_word(state$question,
                                state$quiz,
                                state$wl,
                                all(success))
          state$quiz <- update_quiz(state$question,
                                  state$quiz,
                                  state$wl,
                                  all(success))
          state$icon <- c("nok", "ok")[success + 1]
          state$n_correct <- state$n_correct + all(success)
          state$n_wrong <- state$n_wrong + !all(success)
          state$show_answer <- TRUE
          WordBox:::write_log(state$quiz, "total / correct / wrong / remaining:",
                              state$n_correct + state$n_wrong, "/",
                              state$n_correct, "/",
                              state$n_wrong, "/",
                              nrow(state$quiz))
          shinyjs::enable("gonext")
        }
      } else {
        state$show_answer <- TRUE
        shinyjs::enable("correct")
        shinyjs::enable("wrong")
      }
    }
  })

  # click on actionButton gonext #####
  observeEvent(input$gonext, {
    if (state$show_answer) {
      state$show_answer <- FALSE
      state$i_exercise <- state$i_exercise + 1
      state$reset_ui <- TRUE
      state$retry <- FALSE
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
      # set the icon since this triggers redrawing the UI
      state$icon <- "ok"
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
      state$i_exercise <- state$i_exercise + 1
      # set the icon since this triggers redrawing the UI
      state$icon <- "nok"
      shinyjs::disable("correct")
      shinyjs::disable("wrong")
    }
  })

  # register keys for keyboard input
  WordBox:::register_keys(state)

  # text outputs #####
  output$current_box <- renderText(state$question$box)
  output$question <- renderText({
    # in oral mode, add a comment, if a verb needs to be conjugated
    # state$running must be checked to avoid an error
    paste(state$question$question,
          if (!is.null(state$question) &&
                state$mode == "oral" && state$question$type == "verb")
            "(Konjugation)")
    })
  output$current_group <- renderText(state$question$group)
  output$n_words <- renderText({nrow(state$quiz)})
  output$n_correct <- renderText(state$n_correct)
  output$n_wrong <- renderText({
    if (getOption("wordbox_show_errors")) state$n_wrong
  })
  output$solution <- renderText({
    if (state$show_answer) {
      if (state$question$type == "single") {
        paste(unique(state$question$answers), collapse = "; ")
      } else {
        unique(state$question$answers) %>%
          WordBox:::extract_verb_answers() %>%
          vapply(paste, character(1), collapse = ", ") %>%
          paste(collapse = "; ")
      }
    }
  })

  # reset focus after observers are finished executing
  session$onFlushed(function() {
      if (!is.null(refocus_to)) {
        message("refocus to ", refocus_to)
        shinyjs::js$refocus(refocus_to)
        refocus_to <<- NULL
      }
    },
    once = FALSE)

  # stop app when session ends
  session$onSessionEnded(function() {
    stopApp()
  })

}
