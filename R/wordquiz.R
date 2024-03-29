#' Prepare an Quiz from a Wordlist
#'
#' @param wl a \code{wordlist} object
#' @param direction an integer indicating the direction to be
#'  queried. \code{1} stands for Language1 to Language2 and
#'  \code{2} for the opposite direction.
#' @param quiz_type character indicating the type of quiz to be run.
#'  See 'details'.
#' @param n_words numeric value giving the maximum number of words
#'  to be included in the quiz.
#' @param groups character vector indicating the groups to
#'  be quizzed. If omitted, all groups are included in the quiz.
#' @param core_only logical. Should the quiz only involve words
#'  that are part of the core vocabulary.
#' @param exam_only logical. Should the quiz only involve words
#'  that are part of the current exam.
#' @param log_file If a valid path is provided, the quiz will write
#'  its log to the file. If the file exists and is a log file
#'  produced by WordBox, new entries will be appended. A file
#'  that was not produced by WordBox will not be modified.
#'
#' @details
#' Three types of quizzes can be generated:
#' \describe{
#'   \item{standard}{Words from all boxes are included in the quiz,
#'    if the last correct answer is old enough. How old a correct
#'    answer must be for the word to be quizzed again depends on
#'    the box that it is in. The probability that a word is
#'    quizzed depends on the time that passed since the last
#'    correct answer and the box that it is in. Words in lower
#'    boxes or where the correct answer is further in the past
#'    are quizzed more often.}
#'   \item{training}{All words from all boxes are included in the
#'    quiz with equal probability weight.}
#'   \item{newwords}{Quiz only new words (words in box 1 with less than two
#'   correct answers). Questions are drawn from a list of three words only.
#'   A word is removed from this list and replaced after it has been answered
#'   correctly twice.}
#' }
#' The type of the quiz can be chosen by passing the appropriate
#' value to \code{quiz_type}.
#'
#' @return
#'  a \code{wordquiz} object
#'
#' @aliases wordquiz
#'
#' @export

prepare_quiz <- function(wl, direction,
                         quiz_type = c("standard", "training", "newwords"),
                         groups = NULL,
                         n_words = Inf,
                         core_only = FALSE,
                         exam_only = FALSE,
                         log_file = NULL) {

  direction <- suppressWarnings(as.numeric(direction[1]))
  if (!direction %in% 1:2) {
    stop("Invalid input. direction must be 1 or 2.")
  }
  quiz_type <- match.arg(quiz_type)

  if (n_words < 1) {
    stop("Invalid input. n_words must be larger than 0.")
  }

  # prepare the column names that are relevant for the quiz
  quiz_cols <- list(question = paste0("language", direction),
                    answer   = paste0("language", c(2, 1)[direction]),
                    box      = paste0("box", direction),
                    count    = paste0("count", direction),
                    date     = paste0("date", direction))

  # preparations for verbs: add column word_type which is
  # "single" for simple words, and "verb" for verbs
  wl %<>% dplyr::mutate(
    word_type = dplyr::case_when(
      stringr::str_detect(.data$language2, "^\\\\V") ~ "verb",
      TRUE ~ "single"
    )
  )
  # they are only supported in direction 1
  # => filter them, if direction is 2
  if (direction == 2) {
    wl %<>% dplyr::filter(.data$word_type == "single")
  }

  # create the wordquiz object depending on quiz_type
  if (quiz_type == "training") {
    # in training mode, all words are included with equal weight
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          weight = 1,
                          group = wl$group,
                          type = wl$word_type,
                          core = wl$core,
                          exam = wl$exam)
  } else if (quiz_type == "standard") {
    # in standard mode, words with recent sucess are not quizzed
    # the other words are weighed depending on age and box
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          filter_date = wl[[quiz_cols$date]] +
                                          cfg_days(wl)[wl[[quiz_cols$box]]],
                          weight = compute_weight(wl[[quiz_cols$date]],
                                                  wl[[quiz_cols$box]],
                                                  wl),
                          group = wl$group,
                          type = wl$word_type,
                          core = wl$core,
                          exam = wl$exam) %>%
            dplyr::filter(.data$filter_date <= Sys.Date()) %>%
            dplyr::select(-"filter_date")
  } else if (quiz_type == "newwords") {
    # in newwords mode, all words in box 1 with counts < counts_new
    # are included.
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          box = wl[[quiz_cols$box]],
                          count = wl[[quiz_cols$count]],
                          weight = 0,
                          group = wl$group,
                          type = wl$word_type,
                          core = wl$core,
                          exam = wl$exam) %>%
            dplyr::filter(.data$box == 1, .data$count < cfg_counts_new(wl)) %>%
            dplyr::select(-"box", -"count")
  }

  # add a column with the indices of all correct answers
  # don't mix differente word types ("single", "verb")
  qs <- wl[[quiz_cols$question]]
  wts <- wl$word_type
  quiz$answers <- lapply(quiz$index,
                         function(i) which(qs == qs[i] & wts == wts[i]))

  # filter by groups, if requested
  if (!is.null(groups)) {
    quiz %<>% dplyr::filter(.data$group %in% groups)
  }
  quiz %<>% dplyr::select(-"group")

  # only keep core vocabulary if requested
  if (core_only) {
    quiz %<>% dplyr::filter(.data$core)
  }
  if (exam_only) {
    quiz %<>% dplyr::filter(.data$exam)
  }
  quiz %<>% dplyr::select(-"core", -"exam")

  # reduce the number of words in the quiz to the requested number
  # add a small number to the weights to ensure that they are all positive
  # this is required for quiztype "newwords" where all the weights are zero.
  if (n_words < nrow(quiz)) {
    quiz <- dplyr::sample_n(quiz, n_words, weight = quiz$weight + 1e-6)
  }

  # set the weights if quiz_type is newwords: n_new words
  # must get weight 1 such that they are quizzed first
  # this can't be done before, because filtering for groups
  # and number of words must happen first
  if (quiz_type == "newwords") {
    n_new <- cfg_n_new(wl)
    n_quiz <- nrow(quiz)
    if (n_quiz == 0) {
      i <- integer(0)
    } else if (n_quiz <= n_new) {
      i <- 1:n_quiz
    } else {
      i <- sample(1:n_quiz, n_new)
    }
    quiz$weight[i] <- 1
  }

  # set the log file and write the messages about the new quiz
  langs <- get_languages(wl)
  quiz <- set_logfile(quiz, log_file)
  write_log(quiz, "starting a new quiz from file", get_filename(wl))
  write_log(quiz, "# words in the wordlist:", nrow(wl))
  write_log(quiz, "direction:", langs[direction], ">", rev(langs)[direction])
  write_log(quiz, "quiz type:", quiz_type)
  write_log(quiz, "# words:", nrow(quiz))
  write_log(quiz, "groups:", paste(groups, collapse = ", "))
  write_log(quiz, "core / exam only:", core_only, "/", exam_only)

  # add column names and quiz_type as attribute
  attr(quiz, "cols") <- quiz_cols
  attr(quiz, "type") <- quiz_type

  class(quiz) <- c("wordquiz", class(quiz))
  return(quiz)
}

#' Draw a Question from a wordquiz
#'
#' Draw a question from a quiz.
#'
#' @param quiz the \code{\link{wordquiz}} object from which the
#'  question is drawn.
#' @param wl the \code{\link{wordlist}} object on which the quiz
#'  is based.
#' @param previous the \code{wordquestion} object that was queried
#'  last. If at least two words are left in the quiz, this
#'  question will not be drawn. This can be used to avoid
#'  asking the same question multiple times in a row.
#'
#' @details
#' One of the questions in the quiz is selected based on
#' the weights that are computed in \code{\link{prepare_quiz}}.
#'
#' @return
#' a \code{wordquestion} object
#'
#' @aliases wordquestion
#'
#' @export

draw_question <- function(quiz, wl, previous = NULL) {

  # if the quiz contains no questions, return NULL
  if (nrow(quiz) == 0) return(NULL)

  # a new question is drawn from all the words left
  # in the quiz with the appropriate weights.
  # prepare the indices and remove the index corresponding
  # to the previous question, if it is specified and if
  # there are at least two words left
  i_draw <- 1:nrow(quiz)
  if (!is.null(previous) && nrow(quiz) > 1) {
    i_draw <- i_draw[-previous$i_quiz]
  }

  # draw an index according to the weight,
  # find the appropriate index for the wordlist
  if (length(i_draw) > 1) {
    i <- sample(i_draw, 1, prob = quiz$weight[i_draw])
  } else {
    i <- i_draw
  }
  i_wl <- quiz$index[i]

  # create the wordquestion object
  cols <- get_quiz_cols(quiz)
  question <- list(i_quiz = i,
                   i_wl = i_wl,
                   question = wl[i_wl, cols$question, drop = TRUE],
                   answers = wl[quiz$answers[[i]], cols$answer, drop = TRUE],
                   group = wl[i_wl, "group", drop = TRUE],
                   type = quiz$type[i],
                   box = wl[i_wl, cols$box, drop = TRUE])

  write_log(quiz, "quizzing word:", question$question, "(", question$group, ")")

  class(question) <- "wordquestion"
  return(question)

}


# function to compute the weight used in selecting words to query
compute_weight <- function(date, box, wl) {
  age <- as.numeric(Sys.Date() - date, unit = "days")
  weight <- age/cfg_days(wl)[box] + (cfg_boxes(wl) - box)/cfg_boxes(wl)
  return(weight)
}


#' Extract Attributes From a wordquiz Object
#'
#' Extract the column names that are used for the quiz
#' (\code{quiz_cols}) and the quiz type (\code{quiz_type})
#' from a \code{wordquiz} object.
#'
#' @param quiz a \code{\link{wordquiz}} object
#'
#' @export

get_quiz_cols <- function(quiz) {
  return(attr(quiz, "cols"))
}

#' @rdname get_quiz_cols
#' @export

get_quiz_type <- function(quiz) {
  return(attr(quiz, "type"))
}


#' Check the Answer To a Question
#'
#' @param answer character string giving the answer to the question
#' @param question the \code{\link{wordquestion}} object that was
#'  queried.
#' @param rm_trailing_chars a character of length one containing
#'  characters that should be removed if they appear at the
#'  end of an answer. This can be used to omit rejecting answers
#'  because a key next to Return is hit by accident. For a
#'  Swiss keyboard, \code{rm_trailing_chars = "$"}
#'  is an appropriate choice.
#'
#' @return
#' a logical indicating whether the answer is correct
#'
#' @export

correct_answer <- function(answer, question,
                           rm_trailing_chars = "") {

  answer %<>% trim_char(rm_trailing_chars = rm_trailing_chars) %>%
    standardise_untypeable_characters()
  answers <- question$answers %>%
    standardise_untypeable_characters()

  # how the answer is evaluated depends on the type of the question
  if (question$type == "single") {
    # for "single", simply test that the answer corresponds to one of
    # the correct answers.
    if (length(answer) > 1) {
      warning("Only one answer must be provided for questions of ",
              "type 'single'. Ignoring all but the first.")
      answer <- answer[1]
    }
    answers %<>% trim_char()
    out <- answer %in% answers
  } else if (question$type == "verb") {
    # for "verb", there are totally seven answers that must be checked.
    # if there are multiple valid answers, the first element (infinitiv)
    # is used to pick one. After that, the comparison is simply done
    # element wise.

    if (length(answer) != 7) {
      warning("Exactly 7 answers must be provided for questions of ",
              "type 'verb'. Omitting additional answers or ",
              "filling missing answers by empty strings, respectively.")
      answer <- answer[1:7]
      answer[is.na(answer)] <- ""
    }

    # extract the valid answers and trim
    answers %<>% extract_verb_answers()

    # find the best fitting infinitive form
    infinitives <- vapply(answers, getElement, character(1), 1)
    best_match <- utils::adist(answer[1], infinitives) %>%
      as.vector() %>%
      which.min()
    answers <- answers[[best_match]]

    # compare element-wise
    out <- answer == answers
  }

  out
}


# trim whitespace, collapse multiple whitespace
# and remove certain characters from
# the end of each element in a character vector
# note that some characters must be escaped
# (regex pattern is taken from Hmisc::escapeRegex)
trim_char <- function(x, rm_trailing_chars = "") {

  x %<>% stringr::str_squish()

  if (rm_trailing_chars != "") {
    pattern <- stringr::str_split(rm_trailing_chars, "")[[1]] %>%
      stringr::str_replace_all("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1") %>%
      {paste0("\\s*(", paste(., collapse = "|"), paste0(")$"))}
    x %<>% stringr::str_remove(pattern)
  }

  x
}


# some characters cannot be written capitalised on some
# keyboards. => standardise these characters to not count missing
# capitalisation as error.
# as of now,
standardise_untypeable_characters <- function(x) {
  stringr::str_replace_all(x, "\u00c7", "\u00e7")
}


# auxilliary function to extract the answers for a verb
extract_verb_answers <- function(answers) {
  answers %>% stringr::str_remove_all("(^\\\\V\\()|(\\)$)") %>%
    stringr::str_split(";") %>%
    lapply(trim_char)
}
