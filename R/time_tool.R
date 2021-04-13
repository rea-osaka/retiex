#' classify by year
#'
#' make label for classify by year.
#'
#' @param ts vector of Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' classify_year(as.Date("2020-01-01"))
classify_year <- function(ts){

  # Date型のデータを先に確保する方法
  ans <- rep(as.Date("2000-01-01"), length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- 1
    label_day <- 1
    .month <- lubridate::month(ts[[i]])

    label_date <-
      lubridate::ymd(
        sprintf("%d-%d-%d",
                label_year,
                label_month,
                label_day))

    ans[[i]] <- label_date
  }

  return(ans)
}

#' classify by quarter
#'
#' make label for classify by quarter.
#'
#' @param ts vector of Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' classify_quarter(as.Date("2020-01-01"))
classify_quarter <- function(ts){

  # Date型のデータを先に確保する方法
  ans <- rep(as.Date("2000-01-01"), length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- NULL
    label_day <- 1
    .month <- lubridate::month(ts[[i]])

    ifelse(.month < 4,
           label_month <- 1,
           ifelse(.month < 7,
                  label_month <- 4,
                  ifelse(.month < 10,
                         label_month <- 7,
                         label_month <- 10)))

    label_date <-
      lubridate::ymd(
        sprintf("%d-%d-%d",
                label_year,
                label_month,
                label_day))

    ans[[i]] <- label_date
  }

  return(ans)
}

#' classify by month
#'
#' make label for classify by month.
#'
#' @param ts vector of Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' classify_month(as.Date("2020-01-01"))
classify_month <- function(ts){

  # Date型のデータを先に確保する方法
  ans <- rep(as.Date("2000-01-01"), length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- lubridate::month(ts[[i]])
    label_day <- 1

    label_date <-
      lubridate::ymd(
        sprintf("%d-%d-%d",
                label_year,
                label_month,
                label_day))

    ans[[i]] <- label_date
  }

  return(ans)
}

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
seq_date_by_qarter <- function(min,max){
  #

  # min <= max の検査はしていない
  # 四半期データに変換
  min_quater <- classify_quarter(min)
  max_quater <- classify_quarter(max)

  len <- calc_quarter_num(min_quater, max_quater)

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
seq_date_by_month <- function(min,max){
  #

  # min <= max の検査はしていない
  # 四半期データに変換
  min_month <- classify_month(min)
  max_month <- classify_month(max)

  len <- calc_month_num(min_month, max_month)


  ans <- rep(as.Date("2000-01-01"), len)

  temp_date <- min_month

  for(i in 1:len){
    ans[[i]] <- temp_date
    temp_date <- temp_date + months(1)
  }

  return(ans)
}

calc_month_num <- function(min,max){
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

calc_quarter_num <- function(min,max){
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

