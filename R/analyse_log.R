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


#' @param log a tibble of quiz data created by `analyse_log()`
#' @param y the variable to be used for the y-axis
#' @param colour the variable used for colour
#' @param language language for the plot labels
#' @param interactive create an interactive plot? This requires `plotly` and
#'  is set to `TRUE` by default, if `plotly` is installed. If set to `TRUE` by
#'  the user, the function will ask to install `plotly` if no existing
#'  installation is found.
#'
#' @rdname analyse_log
#' @export

plot_quiz_per_date <- function(
  log,
  y = c("duration", "n_quizzed", "n_correct", "n_wrong"),
  colour = c("file", "direction", "type", "mode", "group"),
  language = c("en", "de"),
  interactive = rlang::is_installed("plotly")) {

  rlang::check_installed("ggplot2")

  y <- match.arg(y) %>%
    rlang::sym()
  colour <- match.arg(colour) %>%
    rlang::sym()
  language <- match.arg(language)

  # data preparation for colour = "group" is different than for the other variables.
  plot_data <- if (colour == "group") {
    # only n_quizzed can be used on the y axis
    y <- rlang::sym("n_quizzed")
    dplyr::bind_cols(
        dplyr::filter(log, lengths(log$words_per_group) > 0) %>%
          dplyr::transmute(date = as.Date(.data$start)),
        dplyr::bind_rows(log$words_per_group)
      ) %>%
      tidyr::pivot_longer(-.data$date,
                          names_to = "group",
                          values_to = "n_quizzed",
                          values_drop_na = TRUE) %>%
      dplyr::mutate(
        tooltip = paste0(
          "Group: ", .data$group, "\n",
          "Date: ", as.character(.data$date), "\n",
          "# Quizzed: ", .data$n_quizzed
        )
      )
  } else {
    log %>%
      # if no words were quizzed, the duration is 0. Remove these lines
      dplyr::filter(.data$duration > 0) %>%
      dplyr::mutate(date = as.Date(.data$start)) %>%
      dplyr::group_by(.data$date, !!colour) %>%
      dplyr::summarise(duration = sum(.data$duration),
                       n_quizzed = sum(.data$n_quizzed),
                       n_correct = sum(.data$n_correct),
                       n_wrong = sum(.data$n_wrong)) %>%
      dplyr::mutate(
        tooltip = paste0(
          as.character(colour), ": ", !!colour, "\n",
          "Date: ", as.character(.data$date), "\n",
          "Duration: ", round(.data$duration, 1), " min.\n",
          "# Quizzed: ", .data$n_quizzed, "\n",
          "# Correct: ", .data$n_correct, "\n",
          "# Wrong: ", .data$n_wrong
        )
      )
  }

  # don't show warning because of "unofficial" text aesthetic
  suppressWarnings(
    p <-  plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$date,
                                   y = !!y,
                                   fill = !!colour,
                                   text = .data$tooltip)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::labs(
        title = "Quiz History",
        x = get_plot_lab("date", language),
        y = get_plot_lab(y, language),
        fill = get_plot_lab(colour, language)
      )
  )

  if (interactive) {
    rlang::check_installed("plotly")
    p <- plotly::ggplotly(p, tooltip = "text")
  }

  p

}


# extract a timestamp from a string
extract_dttm <- function(x) {
  x %>%
    stringr::str_extract("\\d{4}-\\d{2}-\\d{2} +\\d{2}:\\d{2}:\\d{2}") %>%
    as.POSIXct(tz = "CET")
}


# get labels for plot variables
get_plot_lab <- function(x, language = c("en", "de")) {

  language <- match.arg(language)

  lab_translations <- if (language == "en")  {
    c(
      "duration" = "Duration",
      "n_quizzed" = "# Quizzed",
      "n_correct" = "# Correct",
      "n_wrong" = "# Wrong",
      "date" = "Date",
      "file" = "File",
      "direction" = "Direction",
      "type" = "Type",
      "mode" = "Mode",
      "group" = "Group",
      "box" = "Box",
      "n_words" = "# Words"
    )
  } else {
    c(
      "duration" = "Dauer",
      "n_quizzed" = "# W\u00f6rter",
      "n_correct" = "# richtige Antworten",
      "n_wrong" = "# falsche Antworten",
      "date" = "Datum",
      "file" = "Datei",
      "direction" = "Richtung",
      "type" = "Quizart",
      "mode" = "Modus",
      "group" = "Gruppe",
      "box" = "Box",
      "n_words" = "# W\u00f6rter"
    )
  }

  # x may be a symbol => convert to character
  xp <- as.character(x) %>%
    # box and date may come with an 1 or 2 at the end => remove it
    stringr::str_remove("\\d$")
  if (!xp %in% names(lab_translations)) {
    warning("no translation for variable ", x)
    NA_character_
  } else {
    unname(lab_translations[xp])
  }
}

