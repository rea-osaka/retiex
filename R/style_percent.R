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
