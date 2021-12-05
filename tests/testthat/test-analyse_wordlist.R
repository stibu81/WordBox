test_that("test prepare_wl_plot_var()", {
  expect_equal(prepare_wl_plot_var("box", 1), rlang::sym("box1"))
  expect_equal(prepare_wl_plot_var("date", 2), rlang::sym("date2"))
  expect_equal(prepare_wl_plot_var("group", 1), rlang::sym("group"))
})
