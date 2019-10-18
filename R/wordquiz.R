#' Prepare an Quiz from a Wordlist
#'
#' @param wl a \code{wordlist} object
#' @param direction an integer indicating the direction to be
#'  queried. \code{1} stands for Language1 to Language2 and
#'  \code{2} for the opposite direction.
#' @param quiztype character indicating the type of quiz to be run.
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
#' value to \code{quiztype}.
#'
#' @return
#'  a \code{wordquiz} object
#'
#' @aliases wordquiz
#'
#' @export

prepare_quiz <- function(wl, direction,
                         quiztype = c("standard", "training", "newwords"),
                         groups = NULL) {

  direction <- suppressWarnings(as.numeric(direction[1]))
  if (!direction %in% 1:2) {
    stop("Invalid input. direction must be 1 or 2.")
  }
  quiztype <- match.arg(quiztype)

  # prepare the column names that are relevant for the quiz
  quiz_cols <- list(question = paste0("language", direction),
                    answer   = paste0("language", c(2, 1)[direction]),
                    box      = paste0("box", direction),
                    count    = paste0("count", direction),
                    date     = paste0("date", direction))

  # create the wordquiz object
  # in training mode, all words are included with equal weight,
  # otherwise they are weighed by age and words that had recent
  # success are not quizzed.
  if (quiztype == "training") {
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          weight = 1,
                          group = wl$group)
  } else if (quiztype == "standard") {
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          filter_date = wl[[quiz_cols$date]] +
                                          cfg_days(wl)[wl[[quiz_cols$box]]],
                          weight = compute_weight(wl[[quiz_cols$date]],
                                                  wl[[quiz_cols$box]],
                                                  wl),
                          group = wl$group) %>%
            dplyr::filter(.data$filter_date <= Sys.Date()) %>%
            dplyr::select(-"filter_date")
  }

  # add a column with the indices of all correct answers
  qs <- wl[[quiz_cols$question]]
  quiz$answers <- lapply(quiz$index, function(i) which(qs == qs[i]))

  # filter by groups, if requested
  if (!is.null(groups)) {
    quiz %<>% dplyr::filter(.data$group %in% groups)
  }
  quiz %<>% dplyr::select(-"group")

  # add column names as attribute
  attr(quiz, "cols") <- quiz_cols

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

draw_question <- function(quiz, wl) {

  # if the quiz contains no questions, return NULL
  if (nrow(quiz) == 0) return (NULL)

  # draw an index according to the weight,
  # find the appropriate index for the wordlist
  i <- sample(1:nrow(quiz), 1, prob = quiz$weight)
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

# Extract wordlist column names from a wordquiz object

get_quiz_cols <- function(quiz) {
  return(attr(quiz, "cols"))
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
