#' Run Dashboard for Log and Wordlist Analysis
#'
#' @param log_file path to the log file
#' @param wordbox_dir path to the directory with the wordlist files
#' @inheritParams run_wordbox
#'
#' @export

run_wordbox_dashboard <- function(log_file = NULL,
                                  wordbox_dir = NULL,
                                  launch.browser = NULL) {

  rlang::check_installed(c("ggplot2", "plotly", "rmarkdown", "flexdashboard"))

  if (is.null(log_file) && is.null(wordbox_dir)) {
    stop("At least one of log_file and wordlist_dir must be provided.")
  }

  if (!is.null(log_file)) {
    if (!file.exists(log_file)) {
      stop("file ", log_file, " does not exist.")
    }
    log_file <- normalizePath(log_file)
  }
  options(wordbox_log_file = log_file)

  if (!is.null(wordbox_dir)) {
    if (!dir.exists(wordbox_dir)) {
      stop("directory ", wordbox_dir, " does not exist.")
    }
    wordbox_dir <- normalizePath(wordbox_dir)
  }
  options(wordbox_dir = wordbox_dir)

  dashboard_file <- system.file("dashboard", "dashboard.Rmd",
                                package = "WordBox")
  if (dashboard_file == "") {
    stop("Could not find dashboard file. Try re-installing `WordBox`.",
         call. = FALSE)
  }

  # prepare the list with the arguments for shiny::runApp()
  # this is needed because passing list(launch.browser = NULL) to shiny_args fails
  shiny_launch_browser <- if (!is.null(launch.browser)) {
    list(launch.browser = launch.browser)
  }

  rmarkdown::run(dashboard_file, shiny_args = shiny_launch_browser)

}
