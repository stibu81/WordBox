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
  expect_identical(write_wordlist(wl, "test.csv"), wl)
  expect_error(write_wordlist(wl, "test.csv"), "exists and overwrite is FALSE")
  expect_identical(write_wordlist(wl, "test.csv", overwrite = TRUE), wl)
  attr(wl, "file") <- "test.csv"
  expect_equal(read_wordlist("test.csv"), wl)
  unlink("test.csv")
})


test_that("read invalid wordlist files", {
  expect_error(read_wordlist("does_not_exist.csv"), "does not exist.")

  # invalid column names
  wl_bad <- dplyr::rename(wl, bad_name1 = "group", bad_name2 = "box2")
  write_wordlist(wl_bad, "wl_bad.csv", overwrite = TRUE)
  expect_error(read_wordlist("wl_bad.csv"), "Invalid name in column\\(s\\) 3, 9$")

  # wrong number of columns
  wl_bad <- dplyr::select(wl, -"group", -"box2")
  write_wordlist(wl_bad, "wl_bad.csv", overwrite = TRUE)
  expect_error(read_wordlist("wl_bad.csv"), "It must have \\d+ or \\d+ columns")

  # wrong number of columns
  wl_bad <- dplyr::select(wl, "language1", "language2", "core", "exam")
  write_wordlist(wl_bad, "wl_bad.csv", overwrite = TRUE)
  expect_error(read_wordlist("wl_bad.csv"), "It must have \\d+ or \\d+ columns")

  # missing group for first word
  wl_bad <- dplyr::mutate(wl, group = replace(group, 1, NA_character_))
  write_wordlist(wl_bad, "wl_bad.csv", overwrite = TRUE)
  expect_error(read_wordlist("wl_bad.csv"), "group .* must be defined!")

  unlink("wl_bad.csv")
})


test_that("use inexistant config file", {
  expect_warning(
    wl2 <- read_wordlist(wl_file, config_file = "does_not_exist.json"),
    "File does_not_exist.json does not exists. Falling back to default."
  )
  expect_identical(wl2, wl)
})
