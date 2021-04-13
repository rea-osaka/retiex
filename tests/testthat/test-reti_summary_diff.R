test_that("l_add_rolling_tag add first day as tag(col)", {

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
  series <- input$t_date %>% unique() %>% sort(decreasing = T)
  input <- input %>% dplyr::filter(t_date >= series[8])

  # 正しい出力を別の方法で準備
  a1 <-
    input %>%
    dplyr::filter(t_date <= series[1], t_date >= series[4]) %>%
    dplyr::mutate(roll_label = series[1])

  a2 <-
    input %>%
    dplyr::filter(t_date <= series[2], t_date >= series[5]) %>%
    dplyr::mutate(roll_label = series[2])

  a3 <-
    input %>%
    dplyr::filter(t_date <= series[3], t_date >= series[6]) %>%
    dplyr::mutate(roll_label = series[3])

  a4 <-
    input %>%
    dplyr::filter(t_date <= series[4], t_date >= series[7]) %>%
    dplyr::mutate(roll_label = series[4])

  a5 <-
    input %>%
    dplyr::filter(t_date <= series[5], t_date >= series[8]) %>%
    dplyr::mutate(roll_label = series[5])

  # 正しい出力を別の方法で準備
  right_res <-
    dplyr::bind_rows(a1, a2, a3, a4, a5)

  # rolling input

  r_input <-
    data.frame(

    )

  ##########################################################
  #  retiex_summary_diff_by_time(year)
  ##########################################################
  r_input_y <- dplyr::bind_rows(d1,d2)

  tmp_data <- r_input_y %>% retiex_summary_by_time(time = "y")
  tmp2010 <- tmp_data[1,]
  tmp2011 <- tmp_data[2,]

  change_rate_2010 <-
    data.frame(
      cr_mean = NA,
      cr_sd   = NA,
      cr_min  = NA,
      cr_qu1  = NA,
      cr_median = NA,
      cr_qu3  = NA,
      cr_max  = NA,
      cr_count = NA
    )

  change_rate_2011 <-
    data.frame(
      cr_mean = tmp2011$mean / tmp2010$mean - 1,
      cr_sd   = tmp2011$sd / tmp2010$sd - 1,
      cr_min  = tmp2011$min / tmp2010$min - 1,
      cr_qu1  = tmp2011$qu1 / tmp2010$qu1 - 1,
      cr_median = tmp2011$median / tmp2010$median - 1,
      cr_qu3  = tmp2011$qu3 / tmp2010$qu3 - 1,
      cr_max  = tmp2011$max / tmp2010$max - 1,
      cr_count = tmp2011$count / tmp2010$count - 1
    )
  change_rate <- dplyr::bind_rows(change_rate_2010, change_rate_2011)

  change_diff_2010 <-
    data.frame(
      cd_mean = NA,
      cd_sd   = NA,
      cd_min  = NA,
      cd_qu1  = NA,
      cd_median = NA,
      cd_qu3  = NA,
      cd_max  = NA,
      cd_count = NA
    )
  change_diff_2011 <-
    data.frame(
      cd_mean = tmp2011$mean - tmp2010$mean,
      cd_sd   = tmp2011$sd - tmp2010$sd,
      cd_min  = tmp2011$min - tmp2010$min,
      cd_qu1  = tmp2011$qu1 - tmp2010$qu1,
      cd_median = tmp2011$median - tmp2010$median,
      cd_qu3  = tmp2011$qu3 - tmp2010$qu3,
      cd_max  = tmp2011$max - tmp2010$max,
      cd_count = tmp2011$count - tmp2010$count
    )

  change_diff <- dplyr::bind_rows(change_diff_2010,change_diff_2011)

  r_res_y <- dplyr::bind_cols(tmp_data,change_rate,change_diff)


  expect_identical(retiex_summary_diff_by_time(r_input_y, time = "y"), r_res_y)


  ##########################################################
  #  retiex_summary_diff_by_time(quoter)
  ##########################################################
  r_input_q <- dplyr::bind_rows(d1)

  tmp_data <- r_input_q %>% retiex_summary_by_time(time = "q")
  tmp2010_1 <- tmp_data[1,]
  tmp2010_2 <- tmp_data[2,]
  tmp2010_3 <- tmp_data[3,]
  tmp2010_4 <- tmp_data[4,]

  change_rate_2010_1 <-
    data.frame(
      cr_mean = NA,
      cr_sd   = NA,
      cr_min  = NA,
      cr_qu1  = NA,
      cr_median = NA,
      cr_qu3  = NA,
      cr_max  = NA,
      cr_count = NA
    )

  change_rate_2010_2 <-
    data.frame(
      cr_mean = tmp2010_2$mean / tmp2010_1$mean - 1,
      cr_sd   = tmp2010_2$sd / tmp2010_1$sd - 1,
      cr_min  = tmp2010_2$min / tmp2010_1$min - 1,
      cr_qu1  = tmp2010_2$qu1 / tmp2010_1$qu1 - 1,
      cr_median = tmp2010_2$median / tmp2010_1$median - 1,
      cr_qu3  = tmp2010_2$qu3 / tmp2010_1$qu3 - 1,
      cr_max  = tmp2010_2$max / tmp2010_1$max - 1,
      cr_count = tmp2010_2$count / tmp2010_1$count - 1
    )
  change_rate_2010_3 <-
    data.frame(
      cr_mean = tmp2010_3$mean / tmp2010_2$mean - 1,
      cr_sd   = tmp2010_3$sd / tmp2010_2$sd - 1,
      cr_min  = tmp2010_3$min / tmp2010_2$min - 1,
      cr_qu1  = tmp2010_3$qu1 / tmp2010_2$qu1 - 1,
      cr_median = tmp2010_3$median / tmp2010_2$median - 1,
      cr_qu3  = tmp2010_3$qu3 / tmp2010_2$qu3 - 1,
      cr_max  = tmp2010_3$max / tmp2010_2$max - 1,
      cr_count = tmp2010_3$count / tmp2010_2$count - 1
    )

  change_rate_2010_4 <-
    data.frame(
      cr_mean = tmp2010_4$mean / tmp2010_3$mean - 1,
      cr_sd   = tmp2010_4$sd / tmp2010_3$sd - 1,
      cr_min  = tmp2010_4$min / tmp2010_3$min - 1,
      cr_qu1  = tmp2010_4$qu1 / tmp2010_3$qu1 - 1,
      cr_median = tmp2010_4$median / tmp2010_3$median - 1,
      cr_qu3  = tmp2010_4$qu3 / tmp2010_3$qu3 - 1,
      cr_max  = tmp2010_4$max / tmp2010_3$max - 1,
      cr_count = tmp2010_4$count / tmp2010_3$count - 1
    )



  change_rate <-
    dplyr::bind_rows(change_rate_2010_1,
                     change_rate_2010_2,
                     change_rate_2010_3,
                     change_rate_2010_4,
    )

  change_diff_2010_1 <-
    data.frame(
      cd_mean = NA,
      cd_sd   = NA,
      cd_min  = NA,
      cd_qu1  = NA,
      cd_median = NA,
      cd_qu3  = NA,
      cd_max  = NA,
      cd_count = NA
    )

  change_diff_2010_2 <-
    data.frame(
      cd_mean = tmp2010_2$mean - tmp2010_1$mean,
      cd_sd   = tmp2010_2$sd - tmp2010_1$sd,
      cd_min  = tmp2010_2$min - tmp2010_1$min,
      cd_qu1  = tmp2010_2$qu1 - tmp2010_1$qu1,
      cd_median = tmp2010_2$median - tmp2010_1$median,
      cd_qu3  = tmp2010_2$qu3 - tmp2010_1$qu3,
      cd_max  = tmp2010_2$max - tmp2010_1$max,
      cd_count = tmp2010_2$count - tmp2010_1$count
    )

  change_diff_2010_3 <-
    data.frame(
      cd_mean = tmp2010_3$mean - tmp2010_2$mean,
      cd_sd   = tmp2010_3$sd - tmp2010_2$sd,
      cd_min  = tmp2010_3$min - tmp2010_2$min,
      cd_qu1  = tmp2010_3$qu1 - tmp2010_2$qu1,
      cd_median = tmp2010_3$median - tmp2010_2$median,
      cd_qu3  = tmp2010_3$qu3 - tmp2010_2$qu3,
      cd_max  = tmp2010_3$max - tmp2010_2$max,
      cd_count = tmp2010_3$count - tmp2010_2$count
    )
  change_diff_2010_4 <-
    data.frame(
      cd_mean = tmp2010_4$mean - tmp2010_3$mean,
      cd_sd   = tmp2010_4$sd - tmp2010_3$sd,
      cd_min  = tmp2010_4$min - tmp2010_3$min,
      cd_qu1  = tmp2010_4$qu1 - tmp2010_3$qu1,
      cd_median = tmp2010_4$median - tmp2010_3$median,
      cd_qu3  = tmp2010_4$qu3 - tmp2010_3$qu3,
      cd_max  = tmp2010_4$max - tmp2010_3$max,
      cd_count = tmp2010_4$count - tmp2010_3$count
    )


  change_diff <-
    dplyr::bind_rows(change_diff_2010_1,
                     change_diff_2010_2,
                     change_diff_2010_3,
                     change_diff_2010_4,
    )

  r_res_q <- dplyr::bind_cols(tmp_data, change_rate, change_diff)


  expect_identical(retiex_summary_diff_by_time(r_input_q, time = "q"), r_res_q)



  expect_identical(l_add_rolling_tag(input), right_res)
})
