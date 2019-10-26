#' Run the application
#'
#' @param dir character indicating a directory where wordlist
#'  files (csv) are stored. All the csv files in that directory
#'  will be available to be loaded in the application.
#' @param launch.browser logical, if \code{TRUE}, the application
#'  is opened in the system's default browser, if \code{FALSE},
#'  no browser is started. If the argument is omitted, the value
#'  according to the option \code{shiny.launch.browser} is used,
#'  which in RStudio opens the internal shiny viewer.
#' @param config_file character giving the full path to the
#'  configuration file. If omitted, the default configuration is
#'  used. See \code{\link{read_config}} for details
#'  on the config file.
#' @param show_errors should the UI show the number of errors
#'  that were made during a quiz.
#'
#' @export

run_wordbox <- function(dir = NULL, launch.browser = NULL,
                        config_file = NULL,
                        show_errors = TRUE) {

    if (is.null(dir) || !dir.exists(dir)) {
        stop("an existing directory must be provided.")
    }
    options(wordbox_dir = dir)
    appDir <- system.file("shinyApp", package = "WordBox")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `WordBox`.",
           call. = FALSE)
    }
    options(wordbox_cfg_file = config_file,
            wordbox_show_errors = show_errors)

    if (is.null(launch.browser)) {
        launch.browser <- getOption("shiny.launch.browser", interactive())
    }

    shiny::runApp(appDir, display.mode = "normal",
                  launch.browser = launch.browser)
}
