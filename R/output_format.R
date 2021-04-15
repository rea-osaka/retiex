#' Show human readable yen style
#'
#' Change a num into human readable yen style String.
#'
#' @param num int
#' @param keta int
#' @param unit string
#'
#' @return String
#' @export
#'
#' @examples
#' style_yen(12345678)
#' style_yen(10000000, keta = 3, unit = "千円")
style_yen <- function(num, keta = 0, unit = "円"){
  num <- round(num / 10^keta, 0)
  paste0(format(num, scientific =FALSE, big.mark = ","), unit)
}

#' Show human readable percent style
#'
#' Change a double num into human readable percent style String.
#'
#' @param num double
#' @param keta int
#' @param diff logical
#'
#' @return string
#' @export
#'
#' @examples
#' style_percent(0.1)
#' style_percent(0.1, keta = 2, diff = FALSE)
style_percent <- function(num, keta = 1, diff = TRUE){

  # 引数ketaで、小数点以下を決める
  ans_tmp <- round(num * 100, keta)

  if(diff){
    # プラスマイナスを前につけてわかりやすくする
    ans <- ifelse(ans_tmp >= 0,
                  ifelse(ans_tmp == 0,
                         sprintf("±%.*f%%",keta,ans_tmp),
                         sprintf("+%.*f%%",keta,ans_tmp)),
                  #paste0("△",abs(ans_tmp),"%"))
                  sprintf("△%.*f%%",keta,abs(ans_tmp)))
  }else{
    # 特に何もしない
    ans <- sprintf("%.*f%%",keta,ans_tmp)
  }

  return(ans)
}

#' Show human readable quarter date style
#'
#' Change a Date data into
#' human readable quarter date style String.
#'
#' @param date
#'
#' @return string
#' @export
#'
#' @examples
#' style_quarter_date(as.Date("2000-01-01"))
style_quarter_date <- function(date){

  month_num <- lubridate::month(date)
  year_num <- lubridate::year(date)

  qrt_num <- local_calc_quarter_num(month_num)

  ans <- sprintf("%d年第%d四半期", year_num, qrt_num)

  return(ans)
}

#' Show human readable rolling term style
#'
#' Change a Date data into human readable rolling term
#' by quarter date (1 year)style String.
#'
#' @param date
#'
#' @return string
#' @export
#'
#' @examples
#' style_rolling_term_quarter(as.Date("2020-01-01"))
style_rolling_term_quarter <- function(date){

  #入力日付の期から前３期が先頭

  t_month_num <- lubridate::month(date)
  t_year_num <- lubridate::year(date)

  h_month_num <- lubridate::month(date - months(9))
  h_year_num <- lubridate::year(date - months(9))

  ans <-
    sprintf("%d.%dq 〜 %d.%dq",
            h_year_num, local_calc_quarter_num(h_month_num),
            t_year_num, local_calc_quarter_num(t_month_num)
    )

  return(ans)
}


##################################################################
# ローカル関数
##################################################################
local_calc_quarter_num <- function(month_num){
  qrt_num <-
    ifelse((month_num >= 1 & month_num <= 3), 1,
           ifelse((month_num >= 4 & month_num <= 6),2,
                  ifelse((month_num >=7 & month_num <= 9),3,4)))
  return(qrt_num)
}
