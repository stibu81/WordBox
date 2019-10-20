#' Register Success or Failure for a Word in a wordlist
#'
#' After a word has been quizzed, the wordlist must be
#' changed.
#'
#' @param question the \code{\link{wordquestion}} object that
#'  has been quizzed.
#' @param quiz the \code{\link{wordquiz}} from which the question
#'  was taken.
#' @param wl the \code{\link{wordlist}}, on which the quiz is
#'  based and which will be modified.
#' @param success logical indicating whether the question was
#'  answered correctly or not.
#'
#' @details
#' After a word has been quizzed, its entry in the \code{wordlist}
#' must be modified. The modifications affect the columns
#' \code{box}, \code{count}, and \code{date} that belong to
#' the direction that was quizzed.
#'
#' In case of a correct answer, \code{count} is increased by one.
#' If the limit of counts is reached, the word is moved to the
#' next box by increasing \code{box} by one. \code{count} is
#' reset to zero. \code{date}, which indicates the date of the
#' last correct answer, is set to the current date.
#'
#' In case of a wrong answer, \code{count} is set to zero and
#' the word is moved on box back by decreasing \code{box}
#' by one. \code{date} is left unchanged.
#'
#' @return
#' a \code{\link{wordlist}} object
#'
#' @export

mark_word <- function(question, quiz, wl, success) {

  cols <- get_quiz_cols(quiz)
  boxes <- cfg_boxes(wl)
  count <- cfg_counts(wl)[question$box]
  i_wl <- question$i_wl

  # apply success: set date of last success to today,
  # increment count, move to next box, if count limit is reached
  if (success) {
    wl[i_wl, cols$count] %<>% magrittr::add(1)
    wl[i_wl, cols$date] <- Sys.Date()
    if (wl[i_wl, cols$count] >= count) {
      if (question$box < boxes) {
        wl[i_wl, cols$box] %<>% magrittr::add(1)
        wl[i_wl, cols$count] <- 0
      }
    }
  # apply failure: set counts to zero, move one box back
  } else {
    wl[i_wl, cols$count] <- 0
    if (question$box > 1) {
      wl[i_wl, cols$box] %<>% magrittr::subtract(1)
    }
  }

  return(wl)
}

#' Update Quiz After Question Was Answered
#'
#' After a question from a quiz was answered, the
#' \code{wordquiz} object must be updated. This function
#' performs the update depending on the success of the answer
#' and the type of the quiz.
#'
#' @param quiz the \code{\link{wordquiz}} from which the question
#'  was taken and which will be modified.
#' @param wl the \code{\link{wordlist}}, on which the quiz is
#'  based.
#' @inheritParams mark_word
#'
#' @details
#' If an wrong answer was given, the \code{wordquiz} object
#' is returned unchanged. If the answer was correct, the
#' word that has been quizzed is removed from the quiz.
#'
#' If the quiz type is \code{"newwords"}, a word is only
#' removed from the quiz once it has reached a count of 2.
#' Also, when a word is removed, the weights are adapted,
#' such that an additional word is included into the quiz.
#'
#' @export

update_quiz <- function(question, quiz, wl, success) {

  # in case of failure, the quiz is alwoys returned unchanged
  if (!success) return(quiz)

  cols <- get_quiz_cols(quiz)
  type <- get_quiz_type(quiz)

  # two basic situations:
  # if type is "newwords", words are only removed if
  # counts is larger than new_counts, and the number of
  # words with non-vanishing probability is always n_new.
  # Otherwise, words are simply removed from the quiz, if
  # a correct answer was given
  if (type == "newwords") {
    if (wl[[cols$count]][question$i_wl] >= cfg_counts_new(wl)) {
      quiz <- quiz[-question$i_quiz, ]
      i_cand <- which(quiz$weight == 0)
      # important: the case where i_cand is a single number
      # MUST be treated separately, because for n a single
      # integer, sample(n, m) = sample(1:n, m)
      if (length(i_cand) > 1) {
        quiz$weight[sample(i_cand, 1)] <- 1
      } else if (length(i_cand) == 1) {
        quiz$weight[i_cand] <- 1
      }
    }
  } else {
    quiz <- quiz[-question$i_quiz, ]
  }

  return(quiz)
}
