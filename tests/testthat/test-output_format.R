test_that("style_yen shows humanreadable string", {
  expect_identical(style_yen(123456),"123,456円")
  expect_identical(style_yen(123456,3,"千円"),"123千円")
})

test_that("style_percent shows humanreadable string", {
  expect_identical(style_percent(0.1,keta=1,diff = TRUE),"+10.0%")
  expect_identical(style_percent(0.1234,keta=1,diff = TRUE),"+12.3%")
  expect_identical(style_percent(0,keta=2,diff = TRUE),"±0.00%")
  expect_identical(style_percent(-0.01,keta=2,diff = TRUE),"△1.00%")
  expect_identical(style_percent(0.1,keta=2,diff = FALSE),"10.00%")
})

test_that("style quarter date", {
  input <- as.Date(c("2020-01-01","2010-12-31"))
  exp_out <- c("2020年第1四半期","2010年第4四半期")

  expect_equal(style_quarter_date(input), exp_out)
})


test_that("style quarter date", {
  input <- as.Date(c("2020-01-01","2010-12-31"))
  exp_out <- c("2019.2q 〜 2020.1q","2010.1q 〜 2010.4q")

  expect_equal(style_rolling_term_quarter(input), exp_out)
})


test_that("calc quarter num", {
  input <- 1:12
  exp_out <- c(1,1,1,2,2,2,3,3,3,4,4,4)

  expect_equal(local_calc_quarter_num(input),exp_out)
})
