# Tests for wordquiz objects

wl <- read_wordlist(get_wordlist_testfile())

# reference for quiz_cols
ref_qc1 <- list(question = "language1", answer = "language2",
                box = "box1", count = "count1", date = "date1")
ref_qc2 <- list(question = "language2", answer = "language1",
                box = "box2", count = "count2", date = "date2")

test_that("test quiz preparation", {
  quiz1 <- prepare_quiz(wl, 1)
  expect_is(quiz1, "wordquiz")
  expect_identical(ncol(quiz1), 3L)
  expect_identical(quiz1$index, 1:20)
  expect_equal(quiz1$weight, rep(43706.75, 20))
  expect_identical(get_quiz_cols(quiz1), ref_qc1)
  quiz2 <- prepare_quiz(wl, 2)
  expect_is(quiz2, "wordquiz")
  expect_identical(ncol(quiz2), 3L)
  expect_identical(quiz2$index, 1:20)
  expect_equal(quiz2$weight, rep(43706.75, 20))
  expect_identical(get_quiz_cols(quiz2), ref_qc2)
})

test_that("test quiz preparation with options", {
  quiz3 <- prepare_quiz(wl, "1", training = TRUE)
  expect_is(quiz3, "wordquiz")
  expect_identical(ncol(quiz3), 3L)
  expect_identical(quiz3$index, 1:20)
  expect_equal(quiz3$weight, rep(1, 20))
  expect_identical(get_quiz_cols(quiz3), ref_qc1)
  quiz4 <- prepare_quiz(wl, "2", groups = "Parcours 3.1/2")
  expect_is(quiz4, "wordquiz")
  expect_identical(ncol(quiz4), 3L)
  expect_identical(quiz4$index, 13:20)
  expect_equal(quiz4$weight, rep(43706.75, 8))
  expect_identical(get_quiz_cols(quiz4), ref_qc2)
})


test_that("test quiz preparation with errors", {
  expect_error(prepare_quiz(wl, 3), "Invalid input. direction must")
  expect_error(prepare_quiz(wl, "bad"), "Invalid input. direction must")
})


test_that("check quiz preparation that doesn't include all words", {
  wl2 <- wl
  wl2$date1[6:15] <- Sys.Date()
  quiz5 <- prepare_quiz(wl2, 1)
  expect_is(quiz5, "wordquiz")
  expect_identical(ncol(quiz5), 3L)
  expect_identical(quiz5$index, c(1:5, 16:20))
  expect_equal(quiz5$weight, rep(43706.75, 10))
  expect_identical(get_quiz_cols(quiz5), ref_qc1)
})
