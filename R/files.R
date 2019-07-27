#' Read a word list file
#'
#' @param file character indicating the name of the file
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
         "It must have 3 or 7 columns.")
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

  attr(wordlist, "languages") <- languages
  class(wordlist) <- c("wordlist", class(wordlist))

  return(wordlist)
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


# Get the expected names of a wordlist object
get_wordlist_names <- function() {
  c("language1", "language2", "group",
    "box1", "count1", "date1",
    "box2", "count2", "date2")
}


#' Get Configuration Data
#'
#' Read a configuration data from a file or use default values.
#'
#' @param dir character giving a directory path. If
#'  a file called \code{wordbox.cfg} is found in that
#'  folder it is read.
#'
#' @details
#' The function looks for a file called \code{wordbox.cfg}
#' first in the folder indicated by \code{dir} and, if the
#' file is not found, in the user's home. If no config file
#' is found in both locatings, default values are used.
#'
#' @return
#' a list containing the configuration data.
#'
#' @export

get_config <- function(dir) {

  cfg_name <- "wordbox.cfg"
  cfg_dir <- file.path(dir, cfg_name)
  cfg_home <- file.path("~", cfg_name)

  if (file.exists(cfg_dir)) {
    cfg <- read_config(cfg_dir)
  } else if (file.exists(cfg_home)) {
    cfg <- read_config(cfg_home)
  } else {
    cfg <- list(boxes = 4,
                counts = c(4, 3, 3, Inf),
                days = c(1, 7, 30, 90)
                )
  }

  return (cfg)
}


#' Read a Configuration File
#'
#' Read a configuration data from a file or use default values.
#'
#' @param file character giving the path to the file
#'
#' @details
#' The configuration file must contain three lines
#' beginning with the keywords \code{BOXES},
#' \code{COUNTS} and \code{DAYS}.
#'
#' @return
#' a list containing the configuration data.
#'
#' @keywords internal

read_config <- function(file) {
  return (NULL)
}
