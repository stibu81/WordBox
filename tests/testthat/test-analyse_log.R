library(dplyr, warn.conflicts = FALSE)

n_log_rows <- 8
ref_log <- tibble(
  file = "wordlist.csv",
  direction = rep("Deutsch > Englisch", n_log_rows) %>%
                replace(4, "Englisch > Deutsch"),
  type = c("standard", "newwords", "standard", "standard", "standard",
           "standard", "standard", "training"),
  mode = rep("written", n_log_rows) %>%
          replace(c(5, 8), "oral"),
  start = c("2021-12-03 09:58:20", "2021-12-03 10:00:47", "2021-12-03 10:03:32",
            "2021-12-03 10:04:04", "2021-12-03 10:05:11", "2021-12-03 10:05:29",
            "2021-12-03 10:05:40", "2021-12-03 10:53:31") %>%
            as.POSIXct(tz = "CEST"),
  duration = c(1.26666667, 2.25, 0, 0.73333333, 0.11666667, 0, 0.91666667, 0.25),
  n_words = c(10, 10, 0, 8, 5, 5, 10, 10),
  n_quizzed = c(9, 22, NA, 10, 1, NA, 12, 10),
  n_correct = c(6, 16, NA, 8, 0, NA, 8, 6),
  n_wrong = .data$n_quizzed - .data$n_correct,
  # computing n_remaining as n_words - n_correct does not work for type "newword"
  n_remaining = c(4, 0, NA, 0, 4, NA, 2, 4)
)

test_that("test analyse_log()", {
  log_file <- system.file("testfiles/wordbox.log", package = "WordBox")
  expect_true(file.exists(log_file))
  log <- analyse_log(log_file)
  expect_equal(log, ref_log)
})
