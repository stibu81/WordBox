#' Read a Configuration File
#'
#' Read WordBox configuration from a file.
#'
#' @param file character giving the path to the file
#'
#' @details
#' The configuration file must be a JSON file containing a
#' single object with three fields:
#' \describe{
#'   \item{\code{"boxes"}}{a single integer indicating the number of
#'    boxes to be used.}
#'   \item{\code{"counts"}}{an integer array with one less elements
#'    than there are boxes. The numbers indicate for each box, how
#'    often the word must be correctly answered before it is moved
#'    to the next box. For the last box, the value is automatically
#'    set to \code{Inf}, since a word cannot be moved further than
#'    the last box.}
#'   \item{\code{"days"}}{an integer array with one element per box.
#'    For each box, this gives the number of days that a word is not
#'    quizzed, after it has been correctly answered.}
#' }
#'
#' \code{get_default_config_file()} returns the path to the
#' default configuration file. To look at its contents, run
#'
#' \preformatted{
#' file.show(get_default_config_file())
#' }
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


#' @rdname read_config
#' @export

get_default_config_file <- function() {
  system.file(file.path("config", "wordbox.cfg"),
              package = "WordBox")
}
