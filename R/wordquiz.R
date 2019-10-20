#' Prepare an Quiz from a Wordlist
#'
#' @param wl a \code{wordlist} object
#' @param direction an integer indicating the direction to be
#'  queried. \code{1} stands for Language1 to Language2 and
#'  \code{2} for the opposite direction.
#' @param quiz_type character indicating the type of quiz to be run.
#'  See 'details'.
#' @param groups character vector indicating the groups to
#'  be quizzed. If omitted, all groups are included in the quiz.
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
#'   \item{newwords}{safd}
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
                         groups = NULL) {

  direction <- suppressWarnings(as.numeric(direction[1]))
  if (!direction %in% 1:2) {
    stop("Invalid input. direction must be 1 or 2.")
  }
  quiz_type <- match.arg(quiz_type)

  # prepare the column names that are relevant for the quiz
  quiz_cols <- list(question = paste0("language", direction),
                    answer   = paste0("language", c(2, 1)[direction]),
                    box      = paste0("box", direction),
                    count    = paste0("count", direction),
                    date     = paste0("date", direction))

  # create the wordquiz object depending on quiz_type
  if (quiz_type == "training") {
    # in training mode, all words are included with equal weight
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          weight = 1,
                          group = wl$group)
  } else if (quiz_type == "standard") {
    # in standard mode, words with recent sucess are not quizzed
    # the other words are weighed depending on age and box
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          filter_date = wl[[quiz_cols$date]] +
                                          cfg_days(wl)[wl[[quiz_cols$box]]],
                          weight = compute_weight(wl[[quiz_cols$date]],
                                                  wl[[quiz_cols$box]],
                                                  wl),
                          group = wl$group) %>%
            dplyr::filter(.data$filter_date <= Sys.Date()) %>%
            dplyr::select(-"filter_date")
  } else if (quiz_type == "newwords") {
    # in newwords mode, all words in box 1 with counts < counts_new
    # are included.
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          box = wl[[quiz_cols$box]],
                          count = wl[[quiz_cols$count]],
                          weight = 0,
                          group = wl$group) %>%
            dplyr::filter(.data$box == 1, .data$count < cfg_counts_new(wl)) %>%
            dplyr::select(-"box", -"count")
  }

  # add a column with the indices of all correct answers
  qs <- wl[[quiz_cols$question]]
  quiz$answers <- lapply(quiz$index, function(i) which(qs == qs[i]))

  # filter by groups, if requested
  if (!is.null(groups)) {
    quiz %<>% dplyr::filter(.data$group %in% groups)
  }
  quiz %<>% dplyr::select(-"group")

  # set the weights if quiz_type is newwords: n_new words
  # must get weight 1 such that they are quizzed first
  # this can't be done before, because filtering for groups
  # must happen first
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
                   box = wl[i_wl, cols$box, drop = TRUE])

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
#'
#' @return
#' a logical indicating whether the answer is correct
#'
#' @export

correct_answer <- function(answer, question) {
  trimws(answer) %in% question$answers
}
