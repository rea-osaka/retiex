l_add_rolling_tag <- function(reti_data){

  # reti_data$t_dateは四半期期首日付
  # 四半期期首日付データが連続するデータを期待
  # 新しいものから古いもの
  # 連続しているかどうかのチェックは無い
  date_series <-
    unique(reti_data$t_date) %>%
    sort(decreasing = TRUE)

  ans <- NULL

  for(i in 1:(length(date_series)-3)){
    tmp_data <-
      reti_data %>%

      # 期首日付から周期分のデータを抜き出す
      dplyr::filter(t_date <= date_series[[i]],
                    t_date >= date_series[[i+3]]) %>%

      # 期首日付のラベルを貼る
      dplyr::mutate(roll_label = date_series[[i]])

    ans <-
      dplyr::bind_rows(ans, tmp_data)
  }

  return(ans)
}


retiex_summary_diff_by_time <- function(reti_data, time = "y"){

  ans <-
    retiex_summary_by_time(reti_data, time) %>%

    # 対前期間比(増減)
    dplyr::mutate(cr_mean   = mean   / dplyr::lag(mean) - 1,
                  cr_sd     = sd     / dplyr::lag(sd) - 1,
                  cr_min    = min    / dplyr::lag(min) -1,
                  cr_qu1    = qu1    / dplyr::lag(qu1) -1,
                  cr_median = median / dplyr::lag(median) -1,
                  cr_qu3    = qu3    / dplyr::lag(qu3) -1,
                  cr_max    = max    / dplyr::lag(max) -1,
                  cr_count  = count  / dplyr::lag(count) -1,

                  cd_mean   = mean   - dplyr::lag(mean),
                  cd_sd     = sd     - dplyr::lag(sd),
                  cd_min    = min    - dplyr::lag(min),
                  cd_qu1    = qu1    - dplyr::lag(qu1),
                  cd_median = median - dplyr::lag(median),
                  cd_qu3    = qu3    - dplyr::lag(qu3),
                  cd_max    = max    - dplyr::lag(max),
                  cd_count  = count  - dplyr::lag(count))

  return(ans)
}
