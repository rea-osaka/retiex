#' get summary from reti data
#'
#' get summary from reti data by year or quarter term.
#'
#' @param reti_data data.frame
#' @param time string
#'
#' @return data.frame
#' @export
#'
#' @examples
retiex_summary_by_time <- function(reti_data, time = "y"){

  # 年間か、四半期か
  # y,q以外はyとみなす
  if(time == "q"){
    # retiデータの`t_date列`
    d <-
      reti_data %>%
      dplyr::mutate(time = t_date)
  }else{
    # retiデータの`t_year列`
    d <-
      reti_data %>%
      dplyr::mutate(time = t_year)
  }

  ans <-
    get_summary_by_group(
      df = d,
      target_col_name = `取引総額`,
      group_col_name =  `time`)

  return(ans)
}


# 時間が連続することを保証する
summary_by_year <- function(df, nse_date_col, nes_summary_col){

  # 日付列を元に、クラス分け（年毎）したラベルを付加した
  # データフレームを作る
  # 追加される列名は「retiex_date_label」
  # TODO データにない重ならないものを自動で作る？
  date_col <- substitute(nse_date_col)
  date_vector <- df[[date_col]]

  tmp_data <- df %>%
    dplyr::mutate(retiex_date_label = classify_year(date_vector))

  # クラス分けしたデータフレームの内容で
  # （列名は「retiex_date_col」）
  # 指定した列の内容の要約統計量を作る
  target_col <- rlang::enquo(nes_summary_col)

  ans <- tmp_data %>%
    get_summary_by_group(!!target_col, retiex_date_label) %>%
    dplyr::rename(t_date = retiex_date_label)

  rg <- range(ans$t_date)


   return(ans)
}
