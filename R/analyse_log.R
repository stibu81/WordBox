#' Analyse the Log and Plot Quiz History
#'
#' Read in and analyse the log file that is written by the WordBox shiny
#' app. Create a plot of quiz history with various options.
#'
#' @param file path to the log file
#'
#' @return
#' a tibble with one row per quiz for `analyse_log()`. A `ggplot2` or `plotly`
#' plot for `plot_quiz_per_date()`.
#'
#' @export

analyse_log <- function(file) {

  # guess the encoding, if not successful, use UTF-8
  guess_enc <- readr::guess_encoding(file)
  enc <- if (nrow(guess_enc) == 0) {
    stop("The encoding of the file ", file, " could not be determined.")
  } else {
    guess_enc$encoding[1]
  }

  # read the log and split it up in section corresponding to quizzes
  log <- readr::read_lines(file, locale = readr::locale(encoding = enc))
  if (log[1] != "##### WordBox Log File #####") {
    stop(file, " is not a valid WordBox log file.")
  }
  i_start <- stringr::str_which(log, "starting a new quiz from file")
  i_end <- c(i_start[-1] - 1, length(log))
  quiz_logs <- Map(function(i, j) log[i:j], i_start, i_end)

  # compile the data
  lapply(quiz_logs, analyse_one_quiz) %>%
    dplyr::bind_rows()
}


# helper to analyse a single quiz
analyse_one_quiz <- function(ql) {

  start_line <- ql %>%
    stringr::str_subset("starting a new quiz")

  q_file <- start_line %>%
      stringr::str_extract("(?<= from file).*") %>%
      stringr::str_trim() %>%
      basename()

  start_time <- extract_dttm(start_line)
  end_time <- utils::tail(ql, 1) %>% extract_dttm()
  duration <- as.numeric(end_time - start_time, unit = "mins")

  direction <- ql %>%
    stringr::str_subset(" : direction: ") %>%
    stringr::str_extract("\\w+ [<>] \\w+")

  q_type <- ql %>%
    stringr::str_subset(" : quiz type: ") %>%
    stringr::str_extract("(?<=quiz type:).*") %>%
    stringr::str_trim()

  q_mode <- ql %>%
    stringr::str_subset(" : mode: ") %>%
    stringr::str_extract("(?<=mode:).*") %>%
    stringr::str_trim()

  n_words <- ql %>%
    stringr::str_subset(" : # words: ") %>%
    stringr::str_extract("(?<=# words:).*") %>%
    stringr::str_trim() %>%
    as.numeric()

  tcwr <- ql %>%
    stringr::str_subset(" : total / correct / wrong / remaining: ") %>%
    utils::tail(n = 1) %>%
    stringr::str_extract("(?<=remaining:).*") %>%
    stringr::str_split("/") %>%
    unlist() %>%
    as.numeric()

  words_per_group <- ql %>%
    # the word with unit is listed twice: once when it is quizzed ("quizzing word")
    # and once after it was answered ("correct/wrong answer for word").
    # => keep only the lines for answered words to avoid counting twice.
    stringr::str_subset("answer for word:") %>%
    stringr::str_trim() %>%
    # the word may contain brackets and the pattern has to make sure that only
    # the last word in brackets on each line is kept.
    stringr::str_extract("(?<=\\()[^())]*(?=\\)$)") %>%
    stringr::str_trim() %>%
    table() %>%
    as.list()

  dplyr::tibble(
    file = q_file,
    direction = direction,
    type = q_type,
    mode = q_mode,
    start = start_time,
    duration = duration,
    n_words = n_words,
    n_quizzed = tcwr[1],
    n_correct = tcwr[2],
    n_wrong = tcwr[3],
    n_remaining = tcwr[4],
    words_per_group = list(words_per_group)
  )
}


# extract a timestamp from a string
extract_dttm <- function(x) {
  x %>%
    stringr::str_extract("\\d{4}-\\d{2}-\\d{2} +\\d{2}:\\d{2}:\\d{2}") %>%
    as.POSIXct(tz = "CET")
}

