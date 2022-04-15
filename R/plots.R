#' Plot a Wordlist
#'
#' @param wl a \code{wordlist} object
#' @param direction which direction to analyse. This can be given as an integer
#'  (1: query left column; 2: query right column) or as a character string
#'  indicating the query language as it is stored in the \code{wordlist}.
#' @param x the variable to be used for the x-axis
#' @inheritParams plot_quiz_per_date
#'
#' @return
#' A `ggplot2` or `plotly` plot.
#'
#' @export

plot_wordlist_counts <- function(wl,
                                 direction = 1,
                                 x = c("date", "box", "group"),
                                 colour = c("box", "group"),
                                 language = c("en", "de"),
                                 interactive = rlang::is_installed("plotly")) {

  rlang::check_installed("ggplot2")

  # determine the language index (1 or 2). This must be a number such that it
  # can be used on all language-specific columns (language, box, ...)
  if (is.character(direction)) {
    langs <- get_languages(wl) %>% tolower()
    if (!tolower(direction) %in% langs) {
      stop("'", direction, "' is not a valid language for this wordlist. ",
          "Use one of '", langs[1], "' or '", langs[2], "'.")
    }
    direction <- which(langs == tolower(direction))
  } else if (is.numeric(direction)) {
    if (!direction %in% 1:2) {
      stop("invalid input for language. If it is numeric, it must be 1 or 2.")
    }
  }

  x <- match.arg(x) %>%
    prepare_wl_plot_var(direction)
  colour <- match.arg(colour) %>%
    prepare_wl_plot_var(direction)
  language <- match.arg(language)

  plot_data <- wl %>%
    # convert the box columns to character to make them discrete for the plot
    dplyr::mutate(dplyr::across(dplyr::starts_with("box"), as.character)) %>%
    # set date 1900-01-01 to NA since this is the initial date
    dplyr::mutate(dplyr::across(dplyr::starts_with("date"),
                                ~dplyr::if_else(.x == as.Date("1900-01-01"),
                                                as.Date(NA_character_),
                                                .x))
                  ) %>%
    dplyr::count(!!x, !!colour, name = "n_words") %>%
    dplyr::mutate(
      tooltip = paste0(
        get_plot_lab(x, language), ": ", !!x, "\n",
        if (colour != x)
          paste0(get_plot_lab(colour, language), ": ", !!colour, "\n"),
        get_plot_lab("n_words", language), ": ", .data$n_words
      )
    )

  # only use a legend, if colour and x are NOT the same
  use_legend <- if (x != colour) "legend" else "none"

  title <- paste("Wordlist:", get_filename(wl) %>% basename(), ",",
                 get_languages(wl)[direction], ">", get_languages(wl)[-direction])

  # don't show warning because of "inofficial" text aesthetic
  suppressWarnings(
    p <- plot_data %>%
      # remove all rows with missing values in one of the values that will be
      # inside an aesthetic mapping to avoid ggplot2 warnings
      dplyr::filter(!is.na(!!x), !is.na(!!colour)) %>%
      ggplot2::ggplot(ggplot2::aes(x = !!x,
                                   y = .data$n_words,
                                   fill = !!colour,
                                   text = .data$tooltip)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_fill_brewer(palette = "Set1", guide = use_legend) +
      ggplot2::labs(
        title = title,
        x = get_plot_lab(x, language),
        fill = get_plot_lab(colour, language),
        y = get_plot_lab("n_words", language)
      )
  )

  # if groups are on the x-axis, rotate the labels because they can be long
  # and there may be many.
  if (x == "group") {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1))
  }

  if (interactive) {
    rlang::check_installed("plotly")
    p <- plotly::ggplotly(p, tooltip = "text")
    # ggplotly() has a colour legend, even if the plot has none
    # => remove manually if needed
    if (use_legend == "none")
      p <- plotly::hide_legend(p)
  }

  p

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


# function to prepare a variable such that it can be used in
# plot_wordlist():
#   - add language index if needed
#   - convert to symbol
prepare_wl_plot_var <- function(var, direction) {
  if (var %in% c("box", "date")) var <- paste0(var, direction)
  rlang::sym(var)
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
