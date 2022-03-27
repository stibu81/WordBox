# Tests for wordquiz objects
# Note: many tests involving wordquiz objects are contained in
# test-run_wordquiz.R, where a complete quiz is simulated.

wl <- read_wordlist(get_wordlist_testfile())

# set all dates to yesterday for consistent test results
wl$date1 <- Sys.Date() - 1
wl$date2 <- wl$date1

# reference for quiz_cols
ref_qc1 <- list(question = "language1", answer = "language2",
                box = "box1", count = "count1", date = "date1")
ref_qc2 <- list(question = "language2", answer = "language1",
                box = "box2", count = "count2", date = "date2")

test_that("test quiz preparation", {
  quiz <- prepare_quiz(wl, 1)
  expect_s3_class(quiz, "wordquiz")
  expect_identical(ncol(quiz), 4L)
  expect_identical(quiz$index, 1:10)
  expect_equal(quiz$weight, rep(1.75, 10))
  expect_identical(quiz$type, c(rep("single", 8), "verb", "verb"))
  expect_identical(get_quiz_cols(quiz), ref_qc1)
  expect_identical(get_quiz_type(quiz), "standard")

  quiz <- prepare_quiz(wl, 2)
  expect_s3_class(quiz, "wordquiz")
  expect_identical(ncol(quiz), 4L)
  expect_identical(quiz$index, 1:8)
  expect_equal(quiz$weight, rep(1.75, 8))
  expect_identical(quiz$type, rep("single", 8))
  expect_identical(get_quiz_cols(quiz), ref_qc2)
  expect_identical(get_quiz_type(quiz), "standard")
})

test_that("test quiz preparation with options", {
  quiz <- prepare_quiz(wl, "1", quiz_type = "training")
  expect_s3_class(quiz, "wordquiz")
  expect_identical(ncol(quiz), 4L)
  expect_identical(quiz$index, 1:10)
  expect_equal(quiz$weight, rep(1, 10))
  expect_identical(quiz$type, c(rep("single", 8), "verb", "verb"))
  expect_identical(get_quiz_cols(quiz), ref_qc1)
  expect_identical(get_quiz_type(quiz), "training")

  quiz <- prepare_quiz(wl, "2", groups = "Unit2")
  expect_s3_class(quiz, "wordquiz")
  expect_identical(ncol(quiz), 4L)
  expect_identical(quiz$index, 5:8)
  expect_equal(quiz$weight, rep(1.75, 4))
  expect_identical(quiz$type, rep("single", 4))
  expect_identical(get_quiz_cols(quiz), ref_qc2)
  expect_identical(get_quiz_type(quiz), "standard")
})


test_that("test quiz preparation with errors", {
  expect_error(prepare_quiz(wl, 3), "Invalid input. direction must")
  expect_error(prepare_quiz(wl, "bad"), "Invalid input. direction must")
})


test_that("check quiz preparation that doesn't include all words", {
  wl2 <- wl
  wl2$date1[3:6] <- Sys.Date()
  quiz <- prepare_quiz(wl2, 1)
  expect_s3_class(quiz, "wordquiz")
  expect_identical(ncol(quiz), 4L)
  expect_identical(quiz$index, c(1:2, 7:10))
  expect_equal(quiz$weight, rep(1.75, 6))
  expect_identical(quiz$type, c(rep("single", 4), "verb", "verb"))
  expect_identical(get_quiz_cols(quiz), ref_qc1)
})


test_that("check quiz preparation with type newwords", {
  wl3 <- wl
  wl3$box1[7:10] <- 2
  wl3$count1[3:7] <- c(1, 1, 1, 2, 2)
  quiz <- prepare_quiz(wl3, 1, quiz_type = "newwords")
  expect_s3_class(quiz, "wordquiz")
  expect_identical(ncol(quiz), 4L)
  expect_identical(quiz$index, 1:5)
  expect_equal(sum(quiz$weight), 3)
  expect_identical(quiz$type, rep("single", 5))
  expect_identical(get_quiz_cols(quiz), ref_qc1)
  expect_identical(get_quiz_type(quiz), "newwords")

  # special cases: no words, less words than n_new
  quiza <- prepare_quiz(wl3[1:2, ], 1, quiz_type = "newwords")
  expect_identical(quiza$weight, c(1, 1))
})


test_that("check quiz preparation with word limit", {
  expect_error(prepare_quiz(wl, 1, n_words = 0),
               "n_words must be larger than 0.")
  quiz <- prepare_quiz(wl, 1, n_words = 15)
  expect_equal(quiz$index, 1:10)
  quiz <- prepare_quiz(wl, 1, n_words = 4)
  expect_equal(nrow(quiz), 4L)
  quiz <- prepare_quiz(wl, 1, groups = "Unit2", n_words = 3)
  expect_equal(nrow(quiz), 3L)

})


test_that("check quiz preparation with core words", {
  quiz <- prepare_quiz(wl, 1, core_only = TRUE)
  expect_equal(nrow(quiz), 4L)
  quiz <- prepare_quiz(wl, 1, core_only = TRUE, group = "Unit1")
  expect_equal(nrow(quiz), 2L)
  quiz <- prepare_quiz(wl, 1, core_only = TRUE, quiz_type = "training")
  expect_equal(nrow(quiz), 4L)
  quiz <- prepare_quiz(wl, 1, core_only = TRUE, quiz_type = "newwords")
  expect_equal(nrow(quiz), 4L)
})

test_that("check quiz preparation with exam words", {
  quiz <- prepare_quiz(wl, 1, exam_only = TRUE)
  expect_equal(nrow(quiz), 6L)
  quiz <- prepare_quiz(wl, 1, exam_only = TRUE, groups = "Unit2")
  expect_equal(nrow(quiz), 3L)
  quiz <- prepare_quiz(wl, 1, exam_only = TRUE, quiz_type = "training")
  expect_equal(nrow(quiz), 6L)
  quiz <- prepare_quiz(wl, 1, exam_only = TRUE, quiz_type = "newwords")
  expect_equal(nrow(quiz), 6L)
  quiz <- prepare_quiz(wl, 1, exam_only = TRUE,
                       quiz_type = "newwords", n_words = 4)
  expect_equal(nrow(quiz), 4L)
  expect_equal(table(quiz$weight), table(c(0, 1, 1, 1)))
})


test_that("check correct_answer() with trailing characters", {
  question <- list(answers = c("bonjour"), type = "single")
  expect_true(correct_answer("bonjour", question))
  expect_true(correct_answer("  bonjour  ", question))
  expect_false(correct_answer("bonjour $", question))
  expect_true(correct_answer("bonjour $", question, rm_trailing_chars = "$"))
  expect_true(correct_answer("bonjour £ ", question, rm_trailing_chars = "$£"))
  expect_false(correct_answer("bonjour £$ ", question, rm_trailing_chars = "$£"))
})


test_that("check correct_answer() with untypeable characters", {
  question <- list(answers = c("Ça fait"), type = "single")
  expect_true(correct_answer("Ça fait", question))
  expect_true(correct_answer("ça fait", question))
  expect_true(correct_answer(" ça fait  ", question))
  question <- list(answers = c("Ça ça Ça"), type = "single")
  expect_true(correct_answer("ça ça ça", question))
})
