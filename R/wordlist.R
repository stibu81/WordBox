#' Read a word list file
#'
#' @param file character with the full path to the word list file
#' @param config_file character giving the full path to the
#'  configuration file. If omitted, the default configuration is
#'  used. See \code{\link{read_config}} for details
#'  on the config file.
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
#' \item{\code{core}}{is the word part of the core vocabulary?}
#' \item{\code{exam}}{is the word part of the current exam?}
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
#' The full names of the two languages are returned as attribute of
#' the word list and can be obtained using
#' \code{\link{get_languages}}.
#'
#' @aliases wordlist
#'
#' @export

read_wordlist <- function(file, config_file = NULL) {

  if (!file.exists(file))
    stop("The file '", file, "' does not exist.")

  # read in the file and check its properties
  # the first n_base_col columns must always be present
  # the additional n_add_col are optional and will be added
  # by the function if they are missing.
  # the first two columns should always be the names
  # of the languages, the other columns should have
  # fixed names.
  # all columns are read as character and converted later
  n_base_col <- 5
  n_add_col <- 6
  raw <- readr::read_csv(
      file, na = "NA",
      col_types = readr::cols(.default = "c"),
      # don't read lazily because this locks the file on Windows systems
      # and leads to problems when writing afterwards
      lazy = FALSE) %>%
    dplyr::as_tibble()

  # check the number of columns and the column names
  n_col_in <- ncol(raw)
  if (!n_col_in %in% (n_base_col + c(0, n_add_col))) {
    stop(file, " is not a valid wordlist file. ",
         "It must have ", n_base_col, " or ",
         n_base_col + n_add_col, " columns.")
  }
  name_ok <- names(raw)[3:n_col_in] == get_wordlist_names()[3:n_col_in]
  if (any(!name_ok))
    stop(file, " is not a valid wordlist file. ",
         "Invalid name in column(s) ",
         paste(which(!name_ok) + 2, collapse = ", "))

  wordlist <-
    if (n_col_in == n_base_col) {
      na_date <- as.Date(NA_character_, origin = "1970-01-01")
        dplyr::mutate(raw,
          box1 = NA_real_,
          count1 = NA_real_,
          date1 = na_date,
          box2 = NA_real_,
          count2 = NA_real_,
          date2 = na_date)
    } else {
      raw %>%
        dplyr::mutate_at(dplyr::vars(dplyr::matches("^(box|count)")),
                         as.numeric) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::matches("^date")),
                         as.Date)
  }

  # set language attribute, rename first two columns
  languages <- names(wordlist)[1:2] %>%
                magrittr::set_names(paste0("language", 1:2))
  names(wordlist)[1:2] <- names(languages)

  # convert columns core and exam to logical
  wordlist %<>% dplyr::mutate(core = tolower(.data$core) == "x",
                              exam = tolower(.data$exam) == "x")

  if (is.null(config_file)) {
    config_file <- get_default_config_file()
  }
  if (!file.exists(config_file)) {
    warning("File ", config_file, " does not exists. ",
            "Falling back to default.")
    config_file <- get_default_config_file()
  }
  config <- read_config(config_file)
  wordlist %<>% fix_wordlist(config)

  # set attributes
  attr(wordlist, "file") <- file
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

  # convert core and exam back to "x" and ""
  values <- c("", "x")
  wl_write %<>% dplyr::mutate(core = values[.data$core + 1],
                              exam = values[.data$exam + 1])

  names(wl_write)[1:2] <- get_languages(wl)
  readr::write_csv(wl_write, file, na = "")

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


#' Get the File Name from which a Wordlist Was Read
#'
#' @param wl a \code{wordlist} object
#'
#' @return
#' a character with the path to the wordlist file
#'
#' @export

get_filename <- function(wl) {
  attr(wl, "file")
}

# Get the expected names of a wordlist object
get_wordlist_names <- function() {
  c("language1", "language2", "group", "core", "exam",
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

#' @rdname cfg_boxes
#' @export

cfg_counts_new <- function(wl) {
  return(attr(wl, "config")$counts_new)
}

#' @rdname cfg_boxes
#' @export

cfg_n_new <- function(wl) {
  return(attr(wl, "config")$n_new)
}


# Make a wordlist compatible with the configuration

fix_wordlist <- function(wl, config) {

  # fill in missing values
  wl %<>% tidyr::replace_na(list(box1 = 1,
                                 count1 = 0,
                                 date1 = as.Date("1900-01-01"),
                                 box2 = 1,
                                 count2 = 0,
                                 date2 = as.Date("1900-01-01")))

  # fix boxes
  wl %<>% dplyr::mutate(box1 = pmin(.data$box1, config$boxes),
                        box2 = pmin(.data$box2, config$boxes)) %>%
  # fix count: move to next box, if count has reached limit
    dplyr::mutate(inc1 = .data$count1 >= config$counts[.data$box1],
                  inc2 = .data$count2 >= config$counts[.data$box2],
                  box1 = .data$box1 + .data$inc1,
                  box2 = .data$box2 + .data$inc2,
                  count1 = .data$count1 * !.data$inc1,
                  count2 = .data$count2 * !.data$inc2) %>%
    dplyr::select(-"inc1", -"inc2")

  # if the date columns are numeric, convert to date
  if (is.numeric(wl$date1)) wl$date1 %<>% as.Date(origin = "1970-01-01")
  if (is.numeric(wl$date2)) wl$date2 %<>% as.Date(origin = "1970-01-01")

  # replace problematic characters
  repl <- c("\u2026" = "...",
            "\u00ab" = "\"",
            "\u2039" = "'",
            "\u00bb" = "\"",
            "\u203a" = "'",
            "\u201e" = "\"",
            "\u201c" = "\"")
  wl %<>% dplyr::mutate(
    dplyr::across(dplyr::starts_with("language"),
                  ~stringr::str_replace_all(., repl))
    )

  return(wl)

}
