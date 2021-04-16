#' filter by sd
#'
#' 対象となるデータ列の平均値からn倍標準偏差の位置で
#' フィルタリングする
#'
#' @param df data.frame
#' @param nse_col_name 対象となる列の列名
#' @param n 標準偏差の何倍でフィルタするかを整数で指定。デフォルトは4
#' @param check Logical TRUEにすると、範囲外のデータを出力
#'
#' @return data.frame
#' @export
#'
filter_by_sd <- function(df, nse_col_name, n = 4, check = FALSE){
  target_v <- rlang::enquo(nse_col_name)

  mean <- df %>%
    dplyr::summarise(m = mean(!!target_v)) %>% .$m

  sd <- df %>%
    dplyr::summarise(s = stats::sd(!!target_v)) %>% .$s

  min_limit <- mean - sd * n
  max_limit <- mean + sd * n

  ans <- NULL

  if(check == TRUE){

    # 範囲外のものをチェックする
    ans <- df %>%
      dplyr::filter(!!target_v > max_limit | !!target_v < min_limit )

    # 範囲外となるものの比率をコンソールに出力する
    out_rate <- nrow(ans) / nrow(df) * 100
    sprintf("rate of outliers is %2.1f%%", out_rate) %>% print()

  }else{

    ans <- df %>%
      dplyr::filter(!!target_v <= max_limit , !!target_v >= min_limit )
  }

  return(ans)
}
