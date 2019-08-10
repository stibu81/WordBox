#' Read a word list file
#'
#' @param file character indicating the name of the file
#'
#' @details
#' \code{get_wordlist_testfile()} returns the file name of a wordlist
#' file that can be used for tests.
#'
#' @return
#' a wordlist object, which is a data frame with the
#' following columns:
#' \describe{
#' \item{\code{language1}}{words in the first language}
#' \item{\code{language2}}{words in the second langauge}
#' \item{\code{group}}{name of the group that the words belong to}
#' \item{\code{box1}}{the box in which the words are in the mode 1 > 2}
#' \item{\code{count1}}{the number of consecutive successes in the
#'  current box in the mode 1 > 2}
#' \item{\code{date1}}{the date of the last success in the mode 1 > 2}
#' \item{\code{box2}}{the box in which the words are in the mode 2 > 1}
#' \item{\code{count2}}{the number of consecutive successes in the
#'  current box in the mode 2 > 1}
#' \item{\code{date2}}{the date of the last success in the mode 2 > 1}
#' }
#'
#' The full names of the two languages are returned as attribut
#' and can be obtaines using \code{get_languages}.
#'
#' @aliases wordlist
#'
#' @export

read_wordlist <- function(file) {

  if (!file.exists(file))
    stop("The file '", file, "' does not exist.")

  # read in the file and check its properties
  # it should either have 3 or 7 columns
  # the first two columns should always be the names
  # of the languages, the other columns should have
  # fixed names
  # if there are only 3 columns, the missing columns are
  # added
  raw <- suppressMessages(readr::read_csv(file)) %>%
          dplyr::as_tibble()

  if (ncol(raw) == 3) {
    if (names(raw)[3] != get_wordlist_names()[3])
      stop(file, "is not a valid wordlist file. ",
           "The third columns must be called group.")
    na_date <- as.Date(NA_character_, origin = "1970-01-01")
    wordlist <- dplyr::mutate(raw,
                              box1 = NA_integer_,
                              count1 = NA_integer_,
                              date1 = na_date,
                              box2 = NA_integer_,
                              count2 = NA_integer_,
                              date2 = na_date)
  } else if (ncol(raw) == 9) {
    name_ok <- names(raw)[-(1:2)] == get_wordlist_names()[-(1:2)]
    if (any(!name_ok))
      stop(file, " is not a valid wordlist file. ",
           "Invalid name in column(s) ",
           paste(which(name_ok) + 2, collapse = ", "))
    wordlist <- raw
  } else {
    stop(file, " is not a valid wordlist file. ",
         "It must have 3 or 9 columns.")
  }

  # set language attribute, rename first two columns
  languages <- names(wordlist)[1:2] %>%
                magrittr::set_names(paste0("language", 1:2))
  names(wordlist)[1:2] <- names(languages)

  # fill in missing values
  wordlist %<>% tidyr::replace_na(list(box1 = 1,
                                       count1 = 0,
                                       date1 = as.Date("1900-01-01"),
                                       box2 = 1,
                                       count2 = 0,
                                       date2 = as.Date("1900-01-01")))

  # get configuration data
  config <- get_config(dirname(file))

  # set attributes
  attr(wordlist, "config") <- config
  attr(wordlist, "languages") <- languages
  class(wordlist) <- c("wordlist", class(wordlist))

  return(wordlist)
}


#' @rdname read_wordlist
#' @export

get_wordlist_testfile <- function() {
  file <- file.path("testfiles", "wordlist.csv")
  return(system.file(file, package = "WordBox"))
}


#' Save a wordlist to a File
#'
#' Save a \code{wordlist} to a csv file
#'
#' @param wl a \code{wordlist} object
#' @param file character giving the name of the file
#' @param overwrite logical indicating whether an existing file
#'  should be overwritten
#'
#' @export

write_wordlist <- function(wl, file, overwrite = FALSE) {

  if (file.exists(file) && !overwrite)
    stop(file, " exists and overwrite is FALSE.")

  wl_write <- wl
  names(wl_write)[1:2] <- get_languages(wl)
  readr::write_csv(wl_write, file)

  invisible(wl)
}


#' Get the Languages from a wordlist
#'
#' Extract the full names of the two languages that
#' are associated with a \code{wordlist}.
#'
#' @param wl a \code{wordlist} object
#'
#' @return
#' a named vector with the names of the two languages
#'
#' @export

get_languages <- function(wl) {
  return(attr(wl, "languages"))
}


#' Extract the Names of All the Groups in a Wordlist
#'
#' Extract the names of all the groups that are contained
#' in a \code{wordlist}.
#'
#' @param wl a \code{wordlist} object
#'
#' @return
#' a character vector giving the names of the groups.
#' Each name is unique and the vector is sorted.
#'
#' @export

get_groups <- function(wl) {
  return(sort(unique(wl$group)))
}


# Get the expected names of a wordlist object
get_wordlist_names <- function() {
  c("language1", "language2", "group",
    "box1", "count1", "date1",
    "box2", "count2", "date2")
}



#' Obtain configuration from wordlist
#'
#' @param wl a \code{wordlist} object
#'
#' @export

cfg_boxes <- function(wl) {
  return(attr(wl, "config")$boxes)
}

#' @rdname cfg_boxes
#' @export

cfg_counts <- function(wl) {
  return(attr(wl, "config")$counts)
}

#' @rdname cfg_boxes
#' @export

cfg_days <- function(wl) {
  return(attr(wl, "config")$days)
}
