#' get each summary a data by each group
#'
#' get each summary a target numeric data of arbitrary col
#' by each group.
#'
#' @param df data.frame
#' @param target_col_name target col name(non-standard-eval)
#' @param group_col_name  which col as by group (non-standard-eval)
#'
#' @return data.frame
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_summary_by_group(data.frame(g = rep(c(1,2),10), v = 1:20), v, g)
get_summary_by_group <- function(df, target_col_name, group_col_name){

  # nseでの列名の受け取り
  target_name <- rlang::enquo(target_col_name)
  group_name <- rlang::enquo(group_col_name)

  # グループbyするときの順番は無関心
  ans <-
    df %>%
    dplyr::group_by(!!group_name) %>%
    get_summary(!!target_name)

  return(ans)

}
