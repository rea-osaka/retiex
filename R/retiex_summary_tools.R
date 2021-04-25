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


#' get summary from assumed land unit price data
#'
#' get summary from reti data, which was added with
#' assumed land unit price data, by year or quarter term.
#'
#' @param reti_data data.frame
#' @param building_unit_cost Int
#' @param building_durability Int
#' @param heighzone string vector
#' @param report logical
#' @param term string
#'
#' @return data.frame
#' @export
retiex_summary_of_alup <-
  function(reti_data,
           building_unit_cost = 200000,
           building_durability = 40,
           heighzone = NULL,
           report = FALSE,
           term = "q"
           ){

    target_func <- NULL

    if(term == "y" | term == "year"){
      target_func <- get_summary_by_year
    }else{
      target_func <- get_summary_by_quarter
    }

    # asumme land unit price
    temp_added_alup <-
      reti_data %>%
      retiex_add_assumption_land_unit_price(
        building_unit_cost,
        building_durability,
        heighzone)

    if(report){

      local_report_message(temp_added_alup,
                     "retiex_summary_of_alup reported that",
                     building_unit_cost,
                     building_durability,
                     heighzone)
    }

    # return
    temp_added_alup %>%

      # 推定土地単価がNAになる場合を除く
      dplyr::filter(!is.na(assumption_land_unit_price)) %>%

      # 推定土地単価がマイナスになる場合を除く
      dplyr::filter(assumption_land_unit_price >= 0) %>%

      target_func(assumption_land_unit_price, t_date)

  }



#' get rolling summary from assumed land unit price data
#'
#' get rolling summary from reti data, which was added with
#' assumed land unit price data.
#'
#'
#' @param reti_data data.frame
#' @param building_unit_cost Int
#' @param building_durability Int
#' @param heighzone string vector
#' @param report logilal
#'
#' @return data.frame
#' @export
#'
retiex_rolling_summary_of_alup <-
  function(reti_data,
           building_unit_cost = 200000,
           building_durability = 40,
           heighzone = NULL,
           report = FALSE){

    # asumme land unit price
    temp_added_alup <-
      reti_data %>%
      retiex_add_assumption_land_unit_price(
        building_unit_cost,
        building_durability,
        heighzone)


    if(report){

      local_report_message(temp_added_alup,
                     "retiex_rolling_summary_of_alup reported that",
                     building_unit_cost,
                     building_durability,
                     heighzone)

    }

    # return
    temp_added_alup %>%

            # 推定土地単価がNAになる場合を除く
      dplyr::filter(!is.na(assumption_land_unit_price)) %>%

      # 推定土地単価がマイナスになる場合を除く
      dplyr::filter(assumption_land_unit_price >= 0) %>%

      transform_quarter_rolling_format(t_date) %>%

      get_summary_by_quarter(assumption_land_unit_price, roll_label) %>%
      dplyr::mutate(roll_term = style_rolling_term_quarter(.$date)) %>%
      dplyr::select(roll_term, everything())

  }

#############################################################################
# ローカル関数
#############################################################################

local_report_message <- function(df, message,building_unit_cost,building_durability,heighzone){

  na_result <- df %>%
    dplyr::filter(is.na(assumption_land_unit_price)) %>%
    nrow()

  minus_result <- df %>%
    dplyr::filter(assumption_land_unit_price < 0) %>%
    nrow()

  # message
  message(message)
  message("building unit cost : ",building_unit_cost)
  message("building durability: ",building_durability)
  message("added 0.6 zone     : ",heighzone)
  message("How many deleted by NA result: ", na_result)
  message("How many deleted by minus price: ", minus_result)

}
