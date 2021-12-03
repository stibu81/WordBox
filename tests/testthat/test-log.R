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

  # run first quiz (default settings)
  quiz <- prepare_quiz(wl, 2, log_file = log_file)
  expect_equal(get_logfile(quiz), log_file)
  log <- readLines(log_file)
  expect_length(log, 8)
  expect_equal(log[1], "##### WordBox Log File #####")
  expect_match(log[2], "starting a new quiz")
  expect_match(log[4], "direction: Englisch > Deutsch")
  expect_match(log[5], "quiz type: standard")
  expect_match(log[6], paste("# words:", nrow(quiz)))
  expect_match(log[7], "groups:")
  expect_match(log[8], "core / exam only: FALSE / FALSE")

  # run second quiz (different settings)
  quiz2 <- prepare_quiz(wl, 1,
                        quiz_type = "training",
                        groups = "Unit1",
                        core_only = TRUE,
                        log_file = log_file)
  expect_equal(get_logfile(quiz2), log_file)
  log <- readLines(log_file)
  expect_length(log, 15)
  expect_equal(log[1], "##### WordBox Log File #####")
  expect_match(log[9], "starting a new quiz")
  expect_match(log[11], "direction: Deutsch > Englisch")
  expect_match(log[12], "quiz type: training")
  expect_match(log[13], paste("# words:", nrow(quiz2)))
  expect_match(log[14], "groups: Unit1")
  expect_match(log[15], "core / exam only: TRUE / FALSE")

  # check log for quizzed word
  question <- draw_question(quiz2, wl)
  wl <- mark_word(question, quiz2, wl, success = TRUE)
  log <- readLines(log_file)
  expect_match(log[16], paste("quizzing word:", wl$language1[question$i_wl]))
  expect_match(log[17], paste("correct answer for word:", wl$language1[question$i_wl]))
})
