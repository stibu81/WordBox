context("Test reading and writing wordlist files")

wl_file <- get_wordlist_testfile()
languages <- c(language1 = "Deutsch",
               language2 = "Französisch")

wl <- read_wordlist(wl_file)

test_that("read a wordlist file", {
  expect_identical(ncol(wl), 9L)
  expect_is(wl, "wordlist")
  expect_identical(get_languages(wl), languages)
})


test_that("write and read a wordlist file", {
  expect_identical(write_wordlist(wl, "test.csv"), wl)
  expect_error(write_wordlist(wl, "test.csv"), "exists and overwrite is FALSE")
  expect_identical(write_wordlist(wl, "test.csv", overwrite = TRUE), wl)
  expect_equal(read_wordlist("test.csv"), wl)
  unlink("test.csv")
})
