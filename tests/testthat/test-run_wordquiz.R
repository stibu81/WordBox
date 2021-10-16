# Test a run through a quiz

ref_question <- list(i_quiz = 1L, i_wl = 6L,
                     question = "date",
                     answers = c("Dattel", "Datum", "Verabredung"),
                     group = "Unit2",
                     type = "single",
                     box = 1)
class(ref_question) <- "wordquestion"

# prepare the quiz with type standard with just one word
wl <- read_wordlist(get_wordlist_testfile())
wl$date2[-6] <- Sys.Date()
wl$box2[1:3] <- 2
quiz <- prepare_quiz(wl, 2)
quiz2 <- prepare_quiz(wl, 2, "newwords")

# prepare a quiz with the verb (Unit3)
quiz3 <- prepare_quiz(wl, 1, groups = "Unit3")

# answers for verbs
go <- c("to go", "go", "go", "goes", "go", "go", "go")
walk <- c("to walk", "walk", "walk", "walks", "walk", "walk", "walk")

test_that("draw and answer a question", {
  question <- draw_question(quiz, wl)
  expect_is(question, "wordquestion")
  expect_equal(question, ref_question)
  expect_true(correct_answer("Dattel", question))
  expect_true(correct_answer("Datum", question))
  expect_true(correct_answer("  Datum  ", question))
  expect_false(correct_answer("Mond", question))
})


test_that("check answers", {
  question <- draw_question(quiz, wl)
  expect_true(correct_answer("Dattel", question, rm_trailing_chars = "!$"))
  expect_true(correct_answer("Dattel$", question, rm_trailing_chars = "!$"))
  expect_true(correct_answer("Dattel!", question, rm_trailing_chars = "!$"))
  expect_true(correct_answer(" Dattel  $ ", question, rm_trailing_chars = "!$"))
  expect_false(correct_answer("!Dattel", question, rm_trailing_chars = "!$"))
})

test_that("check answers for verb", {
  question <- draw_question(quiz3, wl)
  expect_true(all(correct_answer(go, question)))
  expect_true(all(correct_answer(walk, question)))
  expect_identical(correct_answer(replace(go, 3, "wrong"), question),
                   replace(rep(TRUE, 7), 3, FALSE))
  expect_identical(correct_answer(replace(walk, 1, "to wlak"), question),
                   replace(rep(TRUE, 7), 1, FALSE))
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
  # giving a wrong answer to a word from box 1 must not change anything
  wl2 <- mark_word(question, quiz, wl, FALSE)
  expect_equal(wl, wl2)

  # giving a correct answer, should increase the count and set the date
  wl2 <- mark_word(question, quiz, wl, TRUE)
  expect_equal(wl2$count2[i_wl], 1)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 1)

  # giving multiple correct answers should also change the box
  for (i in 1:3) {
    wl2 <- mark_word(draw_question(quiz, wl2), quiz, wl2, TRUE)
  }
  expect_equal(wl2$count2[i_wl], 0)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 2)

  # change the box twice more
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

  # once the top box is reached, it cannot change again
  for (i in 1:10) {
    wl2 <- mark_word(draw_question(quiz, wl2), quiz, wl2, TRUE)
  }
  expect_equal(wl2$count2[i_wl], 10)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 4)

  # a wrong answer should move the word one box down
  wl2 <- mark_word(draw_question(quiz, wl2), quiz, wl2, FALSE)
  expect_equal(wl2$count2[i_wl], 0)
  expect_equal(wl2$date2[i_wl], Sys.Date())
  expect_equal(wl2$box2[i_wl], 3)
})


test_that("test update_quiz() in standard mode", {
  question <- draw_question(quiz, wl)
  i_wl <- question$i_wl
  # updating the quiz after a wrong answer must not remove the word
  quizb <- update_quiz(question, quiz, wl, FALSE)
  expect_equal(quizb[-3], quiz[-3])
  # after a correct answer, the word must be removed
  wl2 <- mark_word(question, quiz, wl, TRUE)
  quizb <- update_quiz(question, quiz, wl2, TRUE)
  expect_equal(quizb, quiz[-1, ])
})

test_that("test update_quiz() in newword mode", {
  expect_equal(nrow(quiz2), 5)
  expect_equal(sum(quiz2$weight), 3)
  question <- draw_question(quiz2, wl)
  i_wl <- question$i_wl
  # updating the quiz after a wrong answer must not remove the word
  quiz3 <- update_quiz(question, quiz2, wl, FALSE)
  expect_equal(quiz3[-3], quiz2[-3])
  # since this is new word mode, the word is only removed after two correct
  # answers
  wl2 <- mark_word(question, quiz2, wl, TRUE)
  quiz3 <- update_quiz(question, quiz2, wl2, TRUE)
  expect_equal(quiz3[-3], quiz2[-3])
  wl2 <- mark_word(question, quiz3, wl2, TRUE)
  quiz3 <- update_quiz(question, quiz3, wl2, TRUE)
  expect_equal(nrow(quiz3), 4)
  expect_false(i_wl %in% quiz3$index)
  expect_equal(sum(quiz3$weight), 3)

  # go through the other words and answer them correctly twice
  for (n_words in (nrow(quiz3) - 1):0) {
    question <- draw_question(quiz3, wl)
    i_wl <- question$i_wl
    for (i in 1:2) {
      wl2 <- mark_word(question, quiz3, wl2, TRUE)
      quiz3 <- update_quiz(question, quiz3, wl2, TRUE)
    }
    expect_equal(nrow(quiz3), n_words)
    expect_false(i_wl %in% quiz3$index)
    expect_equal(sum(quiz3$weight), min(n_words, nrow(quiz3)))
  }
})


test_that("draw question from empty quiz", {
  expect_null(draw_question(quiz[integer(0), ], wl))
})


test_that("tests with wrong number of answers", {
  single_question <- draw_question(quiz, wl)
  expect_equal(correct_answer(c("Datum", "Dattel"), single_question), TRUE) %>%
    expect_warning("Only one answer must be provided")
  expect_equal(correct_answer(c("Falsch", "Dattel"), single_question), FALSE) %>%
    expect_warning("Only one answer must be provided")
  verb_question <- draw_question(quiz3, wl)
  expect_equal(correct_answer("to go", verb_question),
               c(TRUE, rep(FALSE, 6))) %>%
    expect_warning("Exactly 7 answers must be provided")
  expect_equal(correct_answer(c(go, "too much"), verb_question),
               rep(TRUE, 7)) %>%
    expect_warning("Exactly 7 answers must be provided")
})

