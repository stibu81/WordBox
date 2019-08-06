#' Prepare an Quiz from a Wordlist
#'
#' @param wl a \code{wordlist} object
#' @param direction an integer indicating the direction to be
#'  queried. \code{1} stands for Language1 to Language2 and
#'  \code{2} for the opposite direction.
#'
#'  @return
#'  a \code{wordquiz} object
#'
#' @export

prepare_quiz <- function(wl, direction) {

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
  quiz <- dplyr::tibble(index = 1:nrow(wl),
                        filter_date = wl[[quiz_cols$date]] +
                                        cfg_days(wl)[wl[[quiz_cols$box]]],
                        weight = compute_weight(wl[[quiz_cols$date]],
                                                wl[[quiz_cols$box]],
                                                wl)) %>%
          dplyr::filter(.data$filter_date < Sys.Date()) %>%
          dplyr::select(-"filter_date")

  # add column names as attribute
  attr(quiz, "cols") <- quiz_cols

  class(quiz) <- c("wordquiz", class(quiz))
  return(quiz)
}

#' Draw a Question from a wordquiz
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
