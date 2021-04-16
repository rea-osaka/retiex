test_that("filter by sd", {

  input_v <-  c(1:100,500)
  m <- mean(input_v)
  sd <- stats::sd(input_v)
  n <- 4

  max_l <- m + n * sd
  min_l <- m - n * sd

  input_1 <- data.frame(v = 1:100)
  exp_out_1 <- data.frame(v = c(22:79))

  input_2 <- data.frame(v = c(1:100, 500))
  exp_out_2 <- data.frame(v = 1:100)

  expect_equal(filter_by_sd(input_1,v,1), exp_out_1)
  expect_equal(filter_by_sd(input_2,v), exp_out_2)
})
