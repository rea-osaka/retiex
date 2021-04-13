test_that("retiex_summary_by_time make summary from reti data", {
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
  right_res_y <-
    input %>%
    dplyr::mutate(time = t_year) %>%
    dplyr::group_by(time) %>%
    get_summary(`取引総額`)

  # 正しい出力を別の方法で準備
  right_res_q <-
    input %>%
    dplyr::mutate(time = t_date) %>%
    dplyr::group_by(time) %>%
    get_summary(`取引総額`)

  expect_identical(retiex_summary_by_time(input, time = "y"), right_res_y)
  expect_identical(retiex_summary_by_time(input, time = "q"), right_res_q)
  expect_identical(retiex_summary_by_time(input, time = "a"), right_res_y)


  })

test_that("summary_by_time ", {
  # 入力
  ################################################################
  # summary_by_year test
  ################################################################
  cy_input <- data.frame(
    t_date = c(as.Date("2010-01-01"),
               as.Date("2010-02-01"),
               as.Date("2011-05-01"),
               as.Date("2011-11-01")),

    `取引総額` = sample(100000:200000,4,replace = TRUE)
  )


  cy_output <- dplyr::bind_cols(
    data.frame(
    retiex_date_label = c(as.Date("2010-01-01"),
                          as.Date("2010-01-01"),
                          as.Date("2011-01-01"),
                          as.Date("2011-01-01")),
    cy_input
  )) %>%
    get_summary_by_group(`取引総額`,retiex_date_label) %>%
    dplyr::rename(t_date = retiex_date_label)


  expect_identical(summary_by_year(cy_input,t_date,`取引総額`), cy_output)



  })
