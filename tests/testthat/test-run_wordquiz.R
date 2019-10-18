# Test a run through a quiz

ref_question <- list(i_quiz = 1L, i_wl = 6L,
                     question = "date",
                     answers = c("Dattel", "Datum", "Verabredung"),
                     group = "Unit2", box = 1)
class(ref_question) <- "wordquestion"

# prepare the quiz with just one word
wl <- read_wordlist(get_wordlist_testfile())
wl$date2[-6] <- Sys.Date()
quiz <- prepare_quiz(wl, 2)

test_that("draw and answer a question", {
  question <- draw_question(quiz, wl)
  expect_is(question, "wordquestion")
  expect_equal(question, ref_question)
  expect_true(correct_answer("Dattel", question))
  expect_true(correct_answer("Datum", question))
  expect_true(correct_answer("  Datum  ", question))
  expect_false(correct_answer("Mond", question))
})


test_that("test mark_word()", {
  question <- draw_question(quiz, wl)
  i_wl <- question$i_wl
  wl2 <- mark_word(question, quiz, wl, FALSE)
  expect_equal(wl, wl2)
  wl2 <- mark_word(question, quiz, wl, TRUE)
  expect_equal(wl2$count2[i_wl], 1)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 1)
  for (i in 1:3) {
    wl2 <- mark_word(draw_question(quiz, wl2), quiz, wl2, TRUE)
  }
  expect_equal(wl2$count2[i_wl], 0)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 2)
  for (i in 1:2) {
    wl2 <- mark_word(draw_question(quiz, wl2), quiz, wl2, TRUE)
  }
  expect_equal(wl2$count2[i_wl], 0)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 3)
  for (i in 1:2) {
    wl2 <- mark_word(draw_question(quiz, wl2), quiz, wl2, TRUE)
  }
  expect_equal(wl2$count2[i_wl], 0)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 4)
  for (i in 1:10) {
    wl2 <- mark_word(draw_question(quiz, wl2), quiz, wl2, TRUE)
  }
  expect_equal(wl2$count2[i_wl], 10)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 4)
})
