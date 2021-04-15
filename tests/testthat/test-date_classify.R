test_that("classify by year", {

  ############################################################
  # classify year
  ############################################################
  input <- c(as.Date("2000-02-03"),
             as.Date("2005-04-01"),
             as.Date("2008-08-15"),
             as.Date("2010-12-31"))
  output <- c(as.Date("2000-01-01"),
              as.Date("2005-01-01"),
              as.Date("2008-01-01"),
              as.Date("2010-01-01"))

  expect_identical(classify_year(input), output)
})


test_that("classify by quarter", {

  ############################################################
  # classify quarter
  ############################################################
  input <- c(as.Date("2000-02-03"),
             as.Date("2005-04-01"),
             as.Date("2008-08-15"),
             as.Date("2010-12-31"))
  output <- c(as.Date("2000-01-01"),
              as.Date("2005-04-01"),
              as.Date("2008-07-01"),
              as.Date("2010-10-01"))

  expect_identical(classify_quarter(input), output)

})

test_that("classify by month", {

  ############################################################
  # classify month
  ############################################################

  input <- c(as.Date("2000-02-03"),
             as.Date("2005-04-01"),
             as.Date("2008-08-15"),
             as.Date("2010-12-31"))
  output <- c(as.Date("2000-02-01"),
              as.Date("2005-04-01"),
              as.Date("2008-08-01"),
              as.Date("2010-12-01"))

  expect_identical(classify_month(input), output)

})
