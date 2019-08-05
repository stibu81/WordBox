#' Register Success or Failure for a Word in a wordlist
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
