# Functions for loggin during a quiz

write_log <- function(quiz, ...) {

  logfile <- get_logfile(quiz)

  if (!is.null(logfile)) {
    ts <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%S :")
    message <- paste(...)
    cat(ts, message, "\n",
        file = logfile,
        append = TRUE)
  }

  invisible(NULL)
}

get_logfile <- function(quiz) {
  attr(quiz, "logfile")
}

set_logfile <- function(quiz, logfile) {

  # check validity of the logfile. If the logfile is not valid
  # set the attribute in the quiz to NULL.

  # this message is written to the first line of a new logfile
  # to mark it as a valid WordBox log.
  wordbox_log_first_msg <- "##### WordBox Log File #####"

  # if the file is NULL, no logfile is used
  if (is.null(logfile)) {
    use_logfile <- NULL

  # if the file exists, check that it is marked as WordBox log in the
  # first line to ensure that WordBox never overwrites a file
  # that it should not.
  } else if (file.exists(logfile)) {
    first_line <- trimws(readLines(logfile, n = 1))
    if (first_line != wordbox_log_first_msg) {
      warning("The file ", logfile, " exists, but it is not a WordBox log file.\n",
              "Logging is off.")
      use_logfile <- NULL
    } else {
      use_logfile <- logfile
    }

  # if the file does not exist, try to create it. If that does not work
  # the logfile name is not valid.
  } else {
    use_logfile <- tryCatch({
      cat(wordbox_log_first_msg, "\n",
          file = logfile,
          sep = "")
      logfile
    },
    # placing warning before error catches any warnings produced by cat(), but
    # not the one produced by the error handler below.
    warning = function() {},
    error = function(e) {
      warning("The file ", logfile, " cannot be created with error message '",
              e$message, "'\nLogging is off.")
      NULL
    })
  }

  # set the attribute
  attr(quiz, "logfile") <- use_logfile

  quiz
}
