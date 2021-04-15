#' make sequence date data by year
#'
#' make sequence date data by year.
#'
#' @param min Date
#' @param max Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' seq_date_by_year(as.Date("2000-01-01"),as.Date("2010-01-01"))
seq_date_by_year <- function(min,max){

  # min <= max の検査はしていない
  min_year <- lubridate::year(min)
  max_year <- lubridate::year(max)

  year_v <- min_year:max_year

  ans <- rep(as.Date("2000-01-01"), length(year_v))

  for(i in seq_along(year_v)){
    ans[[i]] <-
      lubridate::ymd(
        sprintf("%d-%d-%d", year_v[[i]],1,1)
      )
  }

  return(ans)
}

#' make sequence date data by quarter
#'
#' make sequence date data by quarter.
#'
#' @param min Date
#' @param max Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' seq_date_by_quarter(as.Date("2000-01-01"),as.Date("2010-01-01"))
seq_date_by_quarter <- function(min,max){
  #

  # min <= max の検査はしていない
  # 四半期データに変換
  min_quater <- classify_quarter(min)
  max_quater <- classify_quarter(max)

  len <- calc_quarter_term(min_quater, max_quater)

  ans <- rep(as.Date("2000-01-01"), len)

  temp_date <- min_quater

  for(i in 1:len){
    ans[[i]] <- temp_date
    temp_date <- temp_date + months(3)
  }

  return(ans)
}

#' make sequence date data by month
#'
#' make sequence date data by month.
#'
#' @param min Date
#' @param max Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' seq_date_by_month(as.Date("2000-01-01"),as.Date("2010-01-01"))
seq_date_by_month <- function(min,max){

  # min <= max の検査はしていない
  # 四半期データに変換
  min_month <- classify_month(min)
  max_month <- classify_month(max)

  len <- calc_month_term(min_month, max_month)

  ans <- rep(as.Date("2000-01-01"), len)

  temp_date <- min_month

  for(i in 1:len){
    ans[[i]] <- temp_date
    temp_date <- temp_date + months(1)
  }

  return(ans)
}
