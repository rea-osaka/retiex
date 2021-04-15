test_that("sequence of year", {
  ############################################################
  # seq date data for year
  ############################################################

  input_min <- as.Date("2009-01-01")
  input_max <- as.Date("2011-01-01")

  output <- c(as.Date("2009-01-01"),
              as.Date("2010-01-01"),
              as.Date("2011-01-01"))

  expect_identical(seq_date_by_year(input_min,input_max), output)

})


test_that("sequence of quarter", {

  ############################################################
  # seq date data for quarter
  ############################################################

  input_min <- as.Date("2009-01-01")
  input_max <- as.Date("2009-02-01")

  output <- c(as.Date("2009-01-01"))

  expect_identical(seq_date_by_quarter(input_min,input_max), output)


  input_min <- as.Date("2009-01-01")
  input_max <- as.Date("2010-09-30")

  output <- c(as.Date("2009-01-01"),
              as.Date("2009-04-01"),
              as.Date("2009-07-01"),
              as.Date("2009-10-01"),
              as.Date("2010-01-01"),
              as.Date("2010-04-01"),
              as.Date("2010-07-01"))

  expect_identical(seq_date_by_quarter(input_min,input_max), output)

})


test_that("sequence of month", {

  ############################################################
  # seq date data for month
  ############################################################

  input_min <- as.Date("2009-01-01")
  input_max <- as.Date("2009-01-11")

  output <- c(as.Date("2009-01-01"))

  expect_identical(seq_date_by_month(input_min,input_max), output)


  input_min <- as.Date("2009-12-31")
  input_max <- as.Date("2010-03-31")

  output <- c(as.Date("2009-12-01"),
              as.Date("2010-01-01"),
              as.Date("2010-02-01"),
              as.Date("2010-03-01"))

  expect_identical(seq_date_by_month(input_min,input_max), output)

})
