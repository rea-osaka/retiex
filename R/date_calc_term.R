#' calculate term by quarter
#'
#' calculate how many terms by quarter.
#'
#' @param min Date
#' @param max Date
#'
#' @return int how many terms
#' @export
#'
#' @examples
#' calc_quarter_term(as.Date("2001-01-01"),as.Date("2001-12-01"))
calc_quarter_term <- function(min,max){

  # 両端入る
  # min maxが同じ時は１期

  # min <= max の検査はしていない
  # 四半期データに変換
  min_quater <- classify_quarter(min)
  max_quater <- classify_quarter(max)

  min_year <- lubridate::year(min_quater)
  max_year <- lubridate::year(max_quater)

  min_left_q <- (12 - lubridate::month(min_quater) + 1) %/% 3
  max_past_q <- (lubridate::month(max_quater) + 2) %/% 3

  ans <- (max_year - min_year - 1) * 4 + min_left_q + max_past_q

  return(ans)
}

#' calculate term by month
#'
#' calculate how many terms by month.
#'
#' @param min Date
#' @param max Date
#'
#' @return int how many terms
#' @export
#'
#' @examples
#' calc_quarter_term(as.Date("2001-01-01"),as.Date("2001-12-01"))
calc_month_term <- function(min,max){
  # 両端入る
  # min maxが同じ時は１期

  # min <= max の検査はしていない
  # 月データに変換
  min_month <- classify_month(min)
  max_month <- classify_month(max)

  min_year <- lubridate::year(min_month)
  max_year <- lubridate::year(max_month)

  min_left_m <- 12 - lubridate::month(min_month) + 1
  max_past_m <- lubridate::month(max_month)

  ans <- (max_year - min_year - 1) * 12 + min_left_m + max_past_m

  return(ans)
}
