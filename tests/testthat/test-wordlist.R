# Test reading, writing and manipulating wordlist files

wl_file <- get_wordlist_testfile()
languages <- c(language1 = "Deutsch",
               language2 = "Französisch")

test_that("check that test file exists", {
  expect_true(file.exists(wl_file))
})

wl <- read_wordlist(wl_file)

test_that("read a wordlist file", {
  expect_identical(ncol(wl), 9L)
  expect_is(wl, "wordlist")
})

test_that("properties of a wordlist", {
  expect_identical(get_languages(wl), languages)
  expect_identical(cfg_boxes(wl), 4)
  expect_identical(cfg_counts(wl), c(3, 2, 2, Inf))
  expect_identical(cfg_days(wl), c(1, 7, 30, 90))
})


test_that("write and read a wordlist file", {
  expect_identical(write_wordlist(wl, "test.csv"), wl)
  expect_error(write_wordlist(wl, "test.csv"), "exists and overwrite is FALSE")
  expect_identical(write_wordlist(wl, "test.csv", overwrite = TRUE), wl)
  expect_equal(read_wordlist("test.csv"), wl)
  unlink("test.csv")
})
