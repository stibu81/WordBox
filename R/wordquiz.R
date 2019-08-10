#' Prepare an Quiz from a Wordlist
#'
#' @param wl a \code{wordlist} object
#' @param direction an integer indicating the direction to be
#'  queried. \code{1} stands for Language1 to Language2 and
#'  \code{2} for the opposite direction.
#' @param training logical indicating whether this is a quiz
#'  in training mode or not. In training mode, all words are
#'  included with equal weight. Otherwise, words are only quizzed,
#'  if their last success is old enough and the probability is
#'  weighed by age and box.
#' @param groups character vector indicating the groups to
#'  be quizzed. If omitted, all groups are included in the quiz.
#'
#' @return
#'  a \code{wordquiz} object
#'
#' @aliases wordquiz
#'
#' @export

prepare_quiz <- function(wl, direction, training = FALSE,
                         groups = NULL) {

  direction <- as.numeric(direction[1])
  if (!direction %in% 1:2) {
    stop("Invalid input. direction must be 1 or 2.")
  }

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
  if (training) {
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          weight = 1,
                          group = wl$group)
  } else {
    quiz <- dplyr::tibble(index = 1:nrow(wl),
                          filter_date = wl[[quiz_cols$date]] +
                                          cfg_days(wl)[wl[[quiz_cols$box]]],
                          weight = compute_weight(wl[[quiz_cols$date]],
                                                  wl[[quiz_cols$box]],
                                                  wl),
                          group = wl$group) %>%
            dplyr::filter(.data$filter_date < Sys.Date()) %>%
            dplyr::select(-"filter_date")
  }

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
                   answer = wl[i_wl, cols$answer, drop = TRUE],
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
