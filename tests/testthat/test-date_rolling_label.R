test_that("transform_quarter_rolling_format ４期連続", {

  input <- data.frame(
    t_date = as.Date(c("2020-01-01",
                       "2020-04-01",
                       "2020-07-01",
                       "2020-10-01"))
  )

  expect_output <- data.frame(
    t_date = as.Date(c("2020-01-01",
                       "2020-04-01",
                       "2020-07-01",
                       "2020-10-01")),

    classify_date = as.Date(c("2020-01-01",
                              "2020-04-01",
                              "2020-07-01",
                              "2020-10-01")),

    roll_label = as.Date(c("2020-10-01",
                           "2020-10-01",
                           "2020-10-01",
                           "2020-10-01"))

  )

  real_output <- transform_quarter_rolling_format(input, t_date)

  expect_equal(real_output, expect_output)

})

test_that("transform_quarter_rolling_format 5期連続", {

  input <- data.frame(
    t_date = as.Date(c("2020-01-01",
                       "2020-04-01",
                       "2020-07-01",
                       "2020-10-01",
                       "2021-01-01"))
  )

  out1 <- data.frame(
    t_date = as.Date(c("2020-01-01",
                       "2020-04-01",
                       "2020-07-01",
                       "2020-10-01")),

    classify_date = as.Date(c("2020-01-01",
                              "2020-04-01",
                              "2020-07-01",
                              "2020-10-01")),

    roll_label = as.Date(c("2020-10-01",
                           "2020-10-01",
                           "2020-10-01",
                           "2020-10-01"))
  )

  out2 <- data.frame(
    t_date = as.Date(c( "2020-04-01",
                        "2020-07-01",
                        "2020-10-01",
                        "2021-01-01")),

    classify_date = as.Date(c( "2020-04-01",
                               "2020-07-01",
                               "2020-10-01",
                               "2021-01-01")),

    roll_label = as.Date(c("2021-01-01",
                           "2021-01-01",
                           "2021-01-01",
                           "2021-01-01"))


  )

  expect_output <- dplyr::bind_rows(out2,out1)

  real_output <- transform_quarter_rolling_format(input, t_date)

  expect_equal(real_output, expect_output)

})
