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
    cfg <- read_config(system.file("config/wordbox.cfg", package = "WordBox"))
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

  if (!file.exists(file)) {
    stop("The config file ", file, " does not exist.")
  }

  cfg <- jsonlite::fromJSON(file)

  # check field names, reorder
  cfg_names <- c("boxes", "counts", "days")
  if (!setequal(names(cfg), cfg_names)) {
    stop("The config file ", file, " is invalid.",
         "It must contain the fields 'boxes', 'counts', and 'days'.")
  }
  cfg <- cfg[cfg_names]

  # boxes must be numeric of length one
  if (!is.numeric(cfg$boxes) || length(cfg$boxes) != 1) {
    stop("boxes must be a single integer")
  }
  cfg$boxes <- floor(cfg$boxes)

  # counts must be numeric of length boxes - 1
  if (!is.numeric(cfg$counts) || length(cfg$counts) != cfg$boxes - 1) {
    stop("counts must be an integer vector with length boxes - 1")
  }
  cfg$counts <- c(floor(cfg$counts), Inf)

  # days must be numeric of length boxes
  if (!is.numeric(cfg$days) || length(cfg$days) != cfg$boxes) {
    stop("days must be an integer vector with length boxes")
  }
  cfg$days <- floor(cfg$days)

  return(cfg)
}
