library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

n_log_rows <- 8
ref_log <- tibble(
  file = "wordlist.csv",
  direction = rep("Deutsch > Englisch", n_log_rows) %>%
                replace(4, "Englisch > Deutsch") %>%
                replace(1, "Deutsch > Französisch"),
  type = c("standard", "newwords", "standard", "standard", "standard",
           "standard", "standard", "training"),
  mode = rep("written", n_log_rows) %>%
          replace(c(5, 8), "oral"),
  start = c("2021-12-03 09:58:20", "2021-12-03 10:00:47", "2021-12-03 10:03:32",
            "2021-12-03 10:04:04", "2021-12-03 10:05:11", "2021-12-03 10:05:29",
            "2021-12-03 10:05:40", "2021-12-03 10:53:31") %>%
            as.POSIXct(tz = "CET"),
  duration = c(1.26666667, 2.25, 0, 0.73333333, 0.11666667, 0, 0.91666667, 0.25),
  n_words = c(10, 10, 0, 8, 5, 5, 10, 10),
  n_quizzed = c(9, 22, NA, 10, 1, NA, 12, 10),
  n_correct = c(6, 16, NA, 8, 0, NA, 8, 6),
  n_wrong = .data$n_quizzed - .data$n_correct,
  # computing n_remaining as n_words - n_correct does not work for type "newword"
  n_remaining = c(4, 0, NA, 0, 4, NA, 2, 4),
  words_per_group = list(list(Unit1 = 4L, Unit2 = 3L, Unit3 = 2L),
                         list(Unit1 = 8L, Unit2 = 7L, Unit3 = 7L),
                         list(),
                         list(Unit1 = 6L, Unit2 = 4L), list(Unit1 = 1L),
                         list(),
                         list(Unit1 = 4L, Unit2 = 3L, Unit3 = 5L),
                         list(Unit1 = 3L, Unit2 = 6L, Unit3 = 1L)
                         )
)

test_that("test analyse_log()", {
  log_file <- system.file("testfiles/wordbox.log", package = "WordBox")
  expect_true(file.exists(log_file))
  expect_equal(analyse_log(log_file), ref_log)
})


test_that("test analyse_log() with different encoding", {
  win_file <- withr::local_file("win_encoding")
  log_file <- system.file("testfiles/wordbox.log", package = "WordBox")
  log_raw <- read_lines(log_file)
  log_win <- iconv(log_raw, "UTF-8", "Windows-1252")
  # cannot use write_lines() which will still write UTF-8
  writeLines(log_win, win_file)
  expect_equal(analyse_log(win_file), ref_log)
})


test_that("test analyse_log() with an encoding that cannot be guessed", {
  bad_file <- withr::local_file("bad_encoding")
  writeBin("\xe324b\xad54c", bad_file)
  expect_error(analyse_log(bad_file),
               "The encoding of the file bad_encoding could not be determined.")
})


test_that("test analyse_log() with an invalid file", {
  bad_file <- withr::local_file("not_a_log")
  write_lines("this is not a WordBox log", bad_file)
  expect_error(analyse_log(bad_file),
               "not_a_log is not a valid WordBox log file.")
})


test_that("test get_plot_lab()", {
  expect_equal(get_plot_lab("date"), "Date")
  expect_equal(get_plot_lab("date1"), "Date")
  expect_equal(get_plot_lab("date", "en"), "Date")
  expect_equal(get_plot_lab("date", "de"), "Datum")
  expect_equal(get_plot_lab("box2"), "Box")
  expect_equal(get_plot_lab("n_correct"), "# Correct")
  expect_equal(get_plot_lab("abc"), NA_character_) %>%
    expect_warning("no translation for variable abc")
  expect_equal(get_plot_lab("abc1"), NA_character_) %>%
    expect_warning("no translation for variable abc1")
  expect_error(get_plot_lab("date", "fr"), "should be one of")
})
