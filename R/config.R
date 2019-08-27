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
                counts = c(3, 2, 2, Inf),
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
