# Test a run through a quiz

ref_question <- list(i_quiz = 1L, i_wl = 6L,
                     question = "date",
                     answers = c("Dattel", "Datum", "Verabredung"),
                     group = "Unit2", box = 1)
class(ref_question) <- "wordquestion"

# prepare the quiz with type standard with just one word
wl <- read_wordlist(get_wordlist_testfile())
wl$date2[-6] <- Sys.Date()
wl$box2[1:4] <- 2
quiz <- prepare_quiz(wl, 2)
quiz2 <- prepare_quiz(wl, 2, "newwords")

test_that("draw and answer a question", {
  question <- draw_question(quiz, wl)
  expect_is(question, "wordquestion")
  expect_equal(question, ref_question)
  expect_true(correct_answer("Dattel", question))
  expect_true(correct_answer("Datum", question))
  expect_true(correct_answer("  Datum  ", question))
  expect_false(correct_answer("Mond", question))
})


test_that("check answers ", {
  question <- draw_question(quiz, wl)
  expect_true(correct_answer("Dattel", question, rm_trailing_chars = "!$"))
  expect_true(correct_answer("Dattel$", question, rm_trailing_chars = "!$"))
  expect_true(correct_answer("Dattel!", question, rm_trailing_chars = "!$"))
  expect_true(correct_answer(" Dattel  $ ", question, rm_trailing_chars = "!$"))
  expect_false(correct_answer("!Dattel", question, rm_trailing_chars = "!$"))
})


test_that("drawing questions using previous", {
  question <- draw_question(quiz2, wl)
  qs <- replicate(100, draw_question(quiz2, wl)$i_wl)
  expect_true(all(quiz2$index[quiz2$weight == 1] %in% qs))
  qs2 <- replicate(100, draw_question(quiz2, wl, question)$i_wl)
  expect_true(!question$i_wl %in% qs2)
  quiz3 <- quiz2[-which(quiz2$weight == 1)[1], ]
  question <- draw_question(quiz3, wl)
  qs2 <- replicate(50, draw_question(quiz3, wl, question)$i_wl)
  expect_true(!question$i_wl %in% qs2)
  question <- draw_question(quiz, wl)
  expect_equal(draw_question(quiz, wl, question), question)
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


test_that("test update_quiz()", {
  expect_equal(nrow(quiz2), 4)
  expect_equal(sum(quiz2$weight), 3)
  question <- draw_question(quiz2, wl)
  i_wl <- question$i_wl
  quiz3 <- update_quiz(question, quiz2, wl, FALSE)
  expect_equal(quiz3[-3], quiz2[-3])
  wl2 <- mark_word(question, quiz2, wl, TRUE)
  quiz3 <- update_quiz(question, quiz2, wl2, TRUE)
  expect_equal(quiz3[-3], quiz2[-3])
  wl2 <- mark_word(question, quiz3, wl2, TRUE)
  quiz3 <- update_quiz(question, quiz3, wl2, TRUE)
  expect_equal(nrow(quiz3), 3)
  expect_false(i_wl %in% quiz3$index)
  expect_equal(sum(quiz3$weight), 3)

  question <- draw_question(quiz3, wl)
  i_wl <- question$i_wl
  for (i in 1:2) {
    wl2 <- mark_word(question, quiz3, wl2, TRUE)
    quiz3 <- update_quiz(question, quiz3, wl2, TRUE)
  }
  expect_equal(nrow(quiz3), 2)
  expect_false(i_wl %in% quiz3$index)
  expect_equal(sum(quiz3$weight), 2)
  question <- draw_question(quiz3, wl)
  i_wl <- question$i_wl
  for (i in 1:2) {
    wl2 <- mark_word(question, quiz3, wl2, TRUE)
    quiz3 <- update_quiz(question, quiz3, wl2, TRUE)
  }
  expect_equal(nrow(quiz3), 1)
  expect_false(i_wl %in% quiz3$index)
  expect_equal(sum(quiz3$weight), 1)
})
