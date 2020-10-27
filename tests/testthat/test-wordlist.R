# Test reading, writing and manipulating wordlist files

wl_file <- get_wordlist_testfile()
languages <- c(language1 = "Deutsch",
               language2 = "Englisch")

test_that("check that test file exists", {
  expect_true(file.exists(wl_file))
})

wl <- read_wordlist(wl_file)

test_that("read a wordlist file", {
  expect_is(wl, "wordlist")
  expect_identical(ncol(wl), 10L)
  expect_identical(nrow(wl), 10L)
})

test_that("properties of a wordlist", {
  expect_identical(get_languages(wl), languages)
  expect_identical(cfg_boxes(wl), 4)
  expect_identical(cfg_counts(wl), c(4, 2, 2, Inf))
  expect_identical(cfg_days(wl), c(1, 7, 30, 90))
  expect_identical(cfg_counts_new(wl), 2)
  expect_identical(cfg_n_new(wl), 3)
})


test_that("write and read a wordlist file", {
  expect_identical(write_wordlist(wl, "test.csv"), wl)
  expect_error(write_wordlist(wl, "test.csv"), "exists and overwrite is FALSE")
  expect_identical(write_wordlist(wl, "test.csv", overwrite = TRUE), wl)
  expect_equal(read_wordlist("test.csv"), wl)
  unlink("test.csv")
})


test_that("read invalid wordlist files", {
  expect_error(read_wordlist("does_not_exist.csv"), "does not exist.")

  wl_bad <- dplyr::rename(wl, bad_name1 = "group", bad_name2 = "box2")
  write_wordlist(wl_bad, "wl_bad.csv", overwrite = TRUE)
  expect_error(read_wordlist("wl_bad.csv"), "Invalid name in column\\(s\\) 3, 8$")

  wl_bad <- dplyr::select(wl, -"group", -"box2")
  write_wordlist(wl_bad, "wl_bad.csv", overwrite = TRUE)
  expect_error(read_wordlist("wl_bad.csv"), "It must have \\d+ or \\d+ columns")

  wl_bad <- dplyr::select(wl, "language1", "language2", "core")
  write_wordlist(wl_bad, "wl_bad.csv", overwrite = TRUE)
  expect_error(read_wordlist("wl_bad.csv"), "It must have \\d+ or \\d+ columns")

  unlink("wl_bad.csv")
})
