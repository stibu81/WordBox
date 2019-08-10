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
    if (wl[i_wl, cols$count] > count) {
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
