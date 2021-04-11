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
