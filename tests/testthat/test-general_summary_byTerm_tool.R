test_that("get_summary_by_year", {

  # 入力
  d1 <- data.frame(
    t_year = rep(2010,20),
    t_date = c(rep(as.Date("2010-01-01"),5),
               rep(as.Date("2010-04-01"),5),
               rep(as.Date("2010-07-01"),5),
               rep(as.Date("2010-10-01"),5)
               ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
    )

  d2 <- data.frame(
    t_year = rep(2011,20),
    t_date = c(rep(as.Date("2011-01-01"),5),
               rep(as.Date("2011-04-01"),5),
               rep(as.Date("2011-07-01"),5),
               rep(as.Date("2011-10-01"),5)
    ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
  )

  d3 <- data.frame(
    t_year = rep(2012,20),
    t_date = c(rep(as.Date("2012-01-01"),5),
               rep(as.Date("2012-04-01"),5),
               rep(as.Date("2012-07-01"),5),
               rep(as.Date("2012-10-01"),5)
    ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
  )

  input <- dplyr::bind_rows(d1,d2,d3)

  # 正しい出力を別の方法で準備
  output <-
    input %>%
    dplyr::mutate(date = classify_year(input$t_date)) %>%
    dplyr::group_by(date) %>%
    get_summary(`取引総額`)


  expect_equal(get_summary_by_year(input, `取引総額`, t_date), output)
})

test_that("get_summary_by_quarter", {

  # 入力
  d1 <- data.frame(
    t_year = rep(2010,20),
    t_date = c(rep(as.Date("2010-01-01"),5),
               rep(as.Date("2010-04-01"),5),
               rep(as.Date("2010-07-01"),5),
               rep(as.Date("2010-10-01"),5)
    ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
  )

  d2 <- data.frame(
    t_year = rep(2011,20),
    t_date = c(rep(as.Date("2011-01-01"),5),
               rep(as.Date("2011-04-01"),5),
               rep(as.Date("2011-07-01"),5),
               rep(as.Date("2011-10-01"),5)
    ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
  )

  d3 <- data.frame(
    t_year = rep(2012,20),
    t_date = c(rep(as.Date("2012-01-01"),5),
               rep(as.Date("2012-04-01"),5),
               rep(as.Date("2012-07-01"),5),
               rep(as.Date("2012-10-01"),5)
    ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
  )

  input <- dplyr::bind_rows(d1,d2,d3)

  # 正しい出力を別の方法で準備
  output <-
    input %>%
    dplyr::mutate(date = classify_quarter(input$t_date)) %>%
    dplyr::group_by(date) %>%
    get_summary(`取引総額`)

  expect_equal(get_summary_by_quarter(input, `取引総額`, t_date), output)
})

test_that("get_summary_by_month", {

  # 入力
  input <- data.frame(
    t_year = rep(2010,20),
    t_date = c(rep(as.Date("2010-01-01"),5),
               rep(as.Date("2010-02-01"),5),
               rep(as.Date("2010-03-01"),5),
               rep(as.Date("2010-04-01"),5)
               ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
    )

  output <-
    input %>%
    dplyr::mutate(date = classify_month(input$t_date)) %>%
    dplyr::group_by(date) %>%
    get_summary(`取引総額`)

  expect_equal(get_summary_by_month(input, `取引総額`, t_date), output)
})


test_that("local_get_summary_by_general_term", {

  # 入力
  d1 <- data.frame(
    t_year = rep(2010,20),
    t_date = c(rep(as.Date("2010-01-01"),5),
               rep(as.Date("2010-04-01"),5),
               rep(as.Date("2010-07-01"),5),
               rep(as.Date("2010-10-01"),5)
               ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
    )

  d2 <- data.frame(
    t_year = rep(2011,20),
    t_date = c(rep(as.Date("2011-01-01"),5),
               rep(as.Date("2011-04-01"),5),
               rep(as.Date("2011-07-01"),5),
               rep(as.Date("2011-10-01"),5)
    ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
  )

  d3 <- data.frame(
    t_year = rep(2012,20),
    t_date = c(rep(as.Date("2012-01-01"),5),
               rep(as.Date("2012-04-01"),5),
               rep(as.Date("2012-07-01"),5),
               rep(as.Date("2012-10-01"),5)
    ),
    `取引総額` = sample(100000:200000,20,replace = TRUE)
  )

  input <- dplyr::bind_rows(d1,d2,d3)

  # 正しい出力を別の方法で準備
  output <-
    input %>%
    dplyr::mutate(date = classify_year(input$t_date)) %>%
    dplyr::group_by(date) %>%
    get_summary(`取引総額`)


  real <- local_get_summary_by_general_term(
    input,
    `取引総額`,
    t_date,
    classify_year,
    seq_date_by_year
  )

  expect_equal(real, output)
})

test_that("loacl_add_row_filled_data", {

  input <- data.frame(
    date = c(as.Date("2000-01-01"),as.Date("2000-02-01")),
    a = c(1,2),
    b = c(1.5,2.5)
  )

  input_date <- as.Date("2000-03-01")

  output <- data.frame(
    date = c(as.Date("2000-01-01"),as.Date("2000-02-01"), input_date),
    a = c(1,2,0),
    b = c(1.5,2.5,0.0)
  )

  expect_equal(local_add_row_filled_data(input, input_date), output)
})

