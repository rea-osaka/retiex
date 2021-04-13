test_that("tiny time tool", {
  ############################################################
  # classify year
  input_date <- c(as.Date("2000-02-03"),
                  as.Date("2000-04-01"),
                  as.Date("2000-08-15"),
                  as.Date("2010-12-31"))
  right_res <- c(as.Date("2000-01-01"),
                  as.Date("2000-01-01"),
                  as.Date("2000-01-01"),
                  as.Date("2010-01-01"))

  expect_identical(classify_year(input_date), right_res)

  ############################################################
  # classify quarter
  input_date <- c(as.Date("2000-02-03"),
                  as.Date("2000-04-01"),
                  as.Date("2000-08-15"),
                  as.Date("2010-12-31"))
  right_res <- c(as.Date("2000-01-01"),
                  as.Date("2000-04-01"),
                  as.Date("2000-07-01"),
                  as.Date("2010-10-01"))

  expect_identical(classify_quarter(input_date), right_res)

  ############################################################
  # classify quarter
  input_date <- c(as.Date("2000-02-03"),
                  as.Date("2000-04-01"),
                  as.Date("2000-08-15"),
                  as.Date("2010-12-31"))
  right_res <- c(as.Date("2000-02-01"),
                  as.Date("2000-04-01"),
                  as.Date("2000-08-01"),
                  as.Date("2010-12-01"))

  expect_identical(classify_month(input_date), right_res)

  ############################################################
  # seq date data for year

  input_min <- as.Date("2009-01-01")
  input_max <- as.Date("2011-01-01")

  right_output <- c(as.Date("2009-01-01"),
                    as.Date("2010-01-01"),
                    as.Date("2011-01-01"))

  seq_date_by_year(input_min,input_max)
  expect_identical(seq_date_by_year(input_min,input_max), right_output)

  ############################################################
  # calc quarter term
  # 四半期の期間を数える（両端はいる）

  input_min <- as.Date("2009-11-01")
  input_max <- as.Date("2010-01-01")

  expect_identical(calc_quarter_num(input_min,input_max), 2)

  ############################################################
  # calc month term
  # 月の期間を数える（両端はいる）

  input_min <- as.Date("2009-11-01")
  input_max <- as.Date("2010-01-01")

  expect_identical(calc_month_num(input_min,input_max), 3)

  ############################################################
  # seq date data for quarter

  input_min <- as.Date("2009-11-01")
  input_max <- as.Date("2010-05-01")

  right_output <- c(as.Date("2009-10-01"),
                    as.Date("2010-01-01"),
                    as.Date("2010-04-01"))

  expect_identical(seq_date_by_qarter(input_min,input_max), right_output)

  ############################################################
  # seq date data for quarter

  input_min <- as.Date("2009-11-01")
  input_max <- as.Date("2010-04-11")

  right_output <- c(as.Date("2009-11-01"),
                    as.Date("2009-12-01"),
                    as.Date("2010-01-01"),
                    as.Date("2010-02-01"),
                    as.Date("2010-03-01"),
                    as.Date("2010-04-01"))

  expect_identical(seq_date_by_month(input_min,input_max), right_output)
})
