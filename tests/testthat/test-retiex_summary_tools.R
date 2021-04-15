test_that("retiex_summary", {

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
  output_year <-
    input %>%
    dplyr::mutate(date = classify_year(input$t_date)) %>%
    dplyr::group_by(date) %>%
    get_summary(`取引総額`)

  # 正しい出力を別の方法で準備
  output_qua <-
    input %>%
    dplyr::mutate(date = classify_quarter(input$t_date)) %>%
    dplyr::group_by(date) %>%
    get_summary(`取引総額`)

  expect_equal(retiex_summary(input, term = "y"), output_year)
  expect_equal(retiex_summary(input, term = "year"), output_year)
  expect_equal(retiex_summary(input), output_qua)
  expect_equal(retiex_summary(input, term = "q"), output_qua)
  expect_equal(retiex_summary(input, term = ""), output_qua)
})

test_that("retiex_rolling_summary", {

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

  real_output <- retiex_rolling_summary(input)

  # 正しい出力を別の方法で準備
  exp_output <-
    input %>%
    transform_quarter_rolling_format(t_date) %>%
    get_summary_by_quarter(`取引総額`, roll_label) %>%
    dplyr::mutate(roll_term = style_rolling_term_quarter(.$date)) %>%
    dplyr::select(roll_term, everything())

  expect_equal(real_output, exp_output)
})
