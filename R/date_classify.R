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
  date_strs <- vector("character",length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])

    date_strs[[i]] <-sprintf("%d-01-01",
                label_year)
  }

  return(as.Date(date_strs))

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

  date_strs <- vector("character",length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- NULL
    .month <- lubridate::month(ts[[i]])

    ifelse(.month < 4,
           label_month <- 1,
           ifelse(.month < 7,
                  label_month <- 4,
                  ifelse(.month < 10,
                         label_month <- 7,
                         label_month <- 10)))

    date_strs[[i]] <-
        sprintf("%d-%d-01",
                label_year,
                label_month)
  }

  return(as.Date(date_strs))
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
  date_strs <- vector("character",length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- lubridate::month(ts[[i]])

    date_strs[[i]] <-
        sprintf("%d-%d-01",
                label_year,
                label_month)

  }

  return(as.Date(date_strs))
}
