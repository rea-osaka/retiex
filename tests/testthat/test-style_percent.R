test_that("style_percent shows humanreadable string", {
  expect_identical(style_percent(0.1,keta=1,diff = TRUE),"+10.0%")
  expect_identical(style_percent(0.1234,keta=1,diff = TRUE),"+12.3%")
  expect_identical(style_percent(0,keta=2,diff = TRUE),"±0.00%")
  expect_identical(style_percent(-0.01,keta=2,diff = TRUE),"△1.00%")
  expect_identical(style_percent(0.1,keta=2,diff = FALSE),"10.00%")
})
