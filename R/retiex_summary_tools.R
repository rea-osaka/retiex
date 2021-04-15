#' get summary from reti data
#'
#' get summary from reti data by year or quarter term.
#'
#' @param reti_data data.frame
#' @param term string
#'
#' @return data.frame
#' @export
#'
retiex_summary <- function(reti_data, term = "q"){

  target_func <- NULL

  if(term == "y" | term == "year"){
    target_func <- get_summary_by_year
  }else{
    target_func <- get_summary_by_quarter
  }

  ans <- reti_data %>%
    target_func(`取引総額`, t_date)

  return(ans)
}


#' get rolling summary from reti data
#'
#' @param reti_data
#'
#' @return data.frame
#' @export
#'
retiex_rolling_summary <- function(reti_data){

  # 移動平均等の算出は、データ連続性等の条件の確保や
  # その各期間と周期の確認等、確認要素が多いので、
  # 汎用化せずに四半期毎に更新され、ある程度の数が揃っている
  # retiデータを対象とした関数のみを用意する
  # TODO もう少し勉強が進めば汎用化を検討する
  # TODO jirei10用に月毎のやつや、半期毎のものを作ってみる

  ans <- reti_data %>%
    transform_quarter_rolling_format(t_date) %>%
    get_summary_by_quarter(`取引総額`,roll_label) %>%
    dplyr::mutate(roll_term = style_rolling_term_quarter(.$date)) %>%
    dplyr::select(roll_term, everything())


  return(ans)
}


###############################################################
# ローカル関数
###############################################################
local_roll_term_string <- function(ts){
  ans <- vector("character", length(ts))
  return(ans)
}
