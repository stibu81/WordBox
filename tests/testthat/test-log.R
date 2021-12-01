library(stringr)

wl <- read_wordlist(get_wordlist_testfile())
log_file <- tempfile("wordbox_test", fileext = ".log")

test_that("create quiz with and without log", {
  withr::local_file(log_file)
  expect_null(get_logfile(prepare_quiz(wl, 2)))
  expect_false(file.exists(log_file))
  expect_equal(get_logfile(prepare_quiz(wl, 2, log_file = log_file)),
               log_file)
  expect_true(file.exists(log_file))
  expect_equal(get_logfile(prepare_quiz(wl, 2, log_file = log_file)),
               log_file)
})

test_that("create log files with error", {
  withr::local_file(log_file)
  bad_log <- file.path(tempdir(), "dir_does_not_exist", "wordbox_test.log")
  expect_warning(
    quiz <- prepare_quiz(wl, 2, log_file = bad_log),
    "cannot be created.*Logging is off"
  )
  expect_null(get_logfile(quiz))
  expect_false(file.exists(log_file))
  writeLines("this is no wordbox log", log_file)
  expect_warning(
    quiz <- prepare_quiz(wl, 2, log_file = log_file),
    "exists, but it is not a WordBox log.*Logging is off"
  )
  expect_null(get_logfile(quiz))
})

test_that("check log file contents", {
  withr::local_file(log_file)
  expect_false(file.exists(log_file))
  quiz <- prepare_quiz(wl, 2, log_file = log_file)
  expect_equal(get_logfile(quiz), log_file)
  log <- readLines(log_file)
  expect_length(log, 8)
  expect_equal(which(log == "##### WordBox Log File #####"), 1)
  expect_equal(str_which(log, "starting a new quiz"), 2)
  expect_equal(get_logfile(prepare_quiz(wl, 2, log_file = log_file)),
               log_file)
  log <- readLines(log_file)
  expect_length(log, 15)
  expect_equal(which(log == "##### WordBox Log File #####"), 1)
  expect_equal(str_which(log, "starting a new quiz"), c(2, 9))
  question <- draw_question(quiz, wl)
  quiz <- mark_word(question, quiz, wl, TRUE)
  log <- readLines(log_file)
  expect_equal(
    str_which(log, paste("word:", wl$language2[question$i_wl])),
    16:17
  )
})
