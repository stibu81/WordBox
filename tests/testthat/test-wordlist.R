# Test reading, writing and manipulating wordlist files
library(dplyr, warn.conflicts = FALSE)

wl_file <- get_wordlist_testfile()
languages <- c(language1 = "Deutsch",
               language2 = "Englisch")

test_that("check that test file exists", {
  expect_true(file.exists(wl_file))
})

wl <- read_wordlist(wl_file)

test_that("read a wordlist file", {
  expect_is(wl, "wordlist")
  expect_identical(ncol(wl), 11L)
  expect_identical(nrow(wl), 10L)
  expect_true(!any(is.na(wl)))
  expect_true(!any(wl[, 1:3] == ""))
})

test_that("properties of a wordlist", {
  expect_identical(get_languages(wl), languages)
  expect_identical(cfg_boxes(wl), 4)
  expect_identical(cfg_counts(wl), c(4, 2, 2, Inf))
  expect_identical(cfg_days(wl), c(1, 7, 30, 90))
  expect_identical(cfg_counts_new(wl), 2)
  expect_identical(cfg_n_new(wl), 3)
  expect_identical(get_filename(wl), wl_file)
  expect_identical(get_groups(wl), paste0("Unit", 1:3))
})


test_that("write and read a wordlist file", {
  wl_tmp <- withr::local_file("test.csv")
  expect_identical(write_wordlist(wl, wl_tmp), wl)
  expect_error(write_wordlist(wl, wl_tmp), "exists and overwrite is FALSE")
  expect_identical(write_wordlist(wl, wl_tmp, overwrite = TRUE), wl)
  attr(wl, "file") <- wl_tmp
  expect_equal(read_wordlist(wl_tmp), wl)
})


test_that("read invalid wordlist files", {
  expect_error(read_wordlist("does_not_exist.csv"), "does not exist.")

  # invalid column names
  wl_tmp <- withr::local_file("wl_bad.csv")
  wl_bad <- rename(wl, bad_name1 = "group", bad_name2 = "box2")
  write_wordlist(wl_bad, wl_tmp, overwrite = TRUE)
  expect_error(read_wordlist(wl_tmp), "Invalid name in column\\(s\\) 3, 9$")

  # wrong number of columns
  wl_bad <- select(wl, -"group", -"box2")
  write_wordlist(wl_bad, wl_tmp, overwrite = TRUE)
  expect_error(read_wordlist(wl_tmp), "It must have \\d+ or \\d+ columns")

  # wrong number of columns
  wl_bad <- select(wl, "language1", "language2", "core", "exam")
  write_wordlist(wl_bad, wl_tmp, overwrite = TRUE)
  expect_error(read_wordlist(wl_tmp), "It must have \\d+ or \\d+ columns")

  # missing group for first word
  wl_bad <- mutate(wl, group = replace(group, 1, NA_character_))
  write_wordlist(wl_bad, wl_tmp, overwrite = TRUE)
  expect_error(read_wordlist(wl_tmp), "group .* must be defined!")
})


test_that("use inexistant config file", {
  expect_warning(
    wl2 <- read_wordlist(wl_file, config_file = "does_not_exist.json"),
    "File does_not_exist.json does not exists. Falling back to default."
  )
  expect_identical(wl2, wl)
})


test_that("read wordlist with missing groups labels", {
  wl_tmp <- withr::local_file("wl_tmp.csv")
  wl_bad <- wl %>%
    mutate(group = if_else(duplicated(group), "", group))
  write_wordlist(wl_bad, wl_tmp, overwrite = TRUE)
  attr(wl, "file") <- wl_tmp
  expect_identical(read_wordlist(wl_tmp), wl)
})


test_that("read wordlist with additional empty column", {
  wl_tmp <- withr::local_file("wl_tmp.csv")

  # 5 + 1 columns
  wl_bad <- readr::read_lines(wl_file) %>%
    paste0(",")
  readr::write_lines(wl_bad, wl_tmp)
  attr(wl, "file") <- wl_tmp
  expect_identical(read_wordlist(wl_tmp), wl)

  # 11 + 3 columns
  write_wordlist(wl, wl_tmp, overwrite = TRUE)
  wl_bad <- readr::read_lines(wl_tmp) %>%
    paste0(",,,")
  readr::write_lines(wl_bad, wl_tmp)
  attr(wl, "file") <- wl_tmp
  expect_identical(read_wordlist(wl_tmp), wl)

  # bad column that is not empty
  wl_bad <- readr::read_lines(wl_file) %>%
    paste0(",")
  wl_bad[5] <- paste0(wl_bad[5], "error")
  readr::write_lines(wl_bad, wl_tmp)
  expect_error(read_wordlist(wl_tmp),
               "It contains columns without header that are not empty.")
})
