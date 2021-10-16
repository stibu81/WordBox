library(jsonlite)

# reference for the contents of the default config file
cfg_ref <- list(boxes = 4,
                counts = c(4, 2, 2, Inf),
                days = c(1, 7, 30, 90),
                counts_new = 2,
                n_new = 3)
config_file <- get_default_config_file()

test_that("test reading of default config file", {
  expect_true(file.exists(config_file))
  cfg <- read_config(config_file)
  expect_is(cfg, "list")
  expect_identical(cfg, cfg_ref)
})

test_that("test reading an invalid config file", {

  expect_error(read_config("does_not_exist.json"),
               "config file does_not_exist.json does not exist.")

  # read the raw test config file
  cfg_raw <- fromJSON(config_file)

  # tempfile to write config to
  cfg_out_file <- tempfile("config", fileext = ".json")

  # wrong number of boxes
  cfg_out <- cfg_raw
  cfg_out$boxes <- 3
  writeLines(jsonlite::toJSON(cfg_out), cfg_out_file)
  expect_error(read_config(cfg_out_file),
               "counts must be an integer vector with length boxes - 1")

  # a field is missing
  cfg_out <- cfg_raw
  cfg_out$n_new <- NULL
  writeLines(jsonlite::toJSON(cfg_out), cfg_out_file)
  expect_error(read_config(cfg_out_file),
               "It must contain the fields")

  # non-numeric value
  cfg_out <- cfg_raw
  cfg_out$counts_new <- "a"
  writeLines(jsonlite::toJSON(cfg_out), cfg_out_file)
  expect_error(read_config(cfg_out_file),
               "counts_new must be a single integer")

  # multiple values instead of length 1
  cfg_out <- cfg_raw
  cfg_out$n_new <- 1:2
  writeLines(jsonlite::toJSON(cfg_out), cfg_out_file)
  expect_error(read_config(cfg_out_file),
               "n_new must be a single integer")
  cfg_out <- cfg_raw
  cfg_out$boxes <- "a"
  writeLines(jsonlite::toJSON(cfg_out), cfg_out_file)
  expect_error(read_config(cfg_out_file),
               "boxes must be a single integer")

  # wrong number of values for days
  cfg_out <- cfg_raw
  cfg_out$days <- 1:(cfg_out$boxes + 1)
  writeLines(jsonlite::toJSON(cfg_out), cfg_out_file)
  expect_error(read_config(cfg_out_file),
               "days must be an integer vector with length boxes")
})
