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
  expect_identical(ncol(wl), 9L)
  expect_identical(nrow(wl), 9L)
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
