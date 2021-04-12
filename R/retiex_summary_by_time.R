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
