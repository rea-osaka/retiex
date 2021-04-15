#' get summary in a arbitrary numeric col
#'
#' get summary on numeric data of a arbitrary col.
#'
#' @param df data.frame
#' @param nse_col col-name(non-standard-eval)
#'
#' @return data.frame
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' get_summary(data.frame(t = 1:10), t)
get_summary <- function(df, nse_col){

  # enquoされたものは、dplyerの引数で!!つける
  target_col_name <- rlang::enquo(nse_col)

  ans <-
    df %>%
    dplyr::summarise(
      mean = mean(!!target_col_name),
      sd = stats::sd(!!target_col_name),
      min = summary(!!target_col_name)[1] %>% as.numeric(),
      qu1 = summary(!!target_col_name)[2] %>% as.numeric(),
      median = summary(!!target_col_name)[3] %>% as.numeric(),
      qu3 = summary(!!target_col_name)[5] %>% as.numeric(),
      max = summary(!!target_col_name)[6] %>% as.numeric(),
      count = dplyr::n()
    )
  return(ans)
}

#' get each summary by each group
#'
#' get each summary about target numeric data of arbitrary col
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

  ans <-
    df %>%
    dplyr::group_by(!!group_name) %>%
    get_summary(!!target_name)

  return(ans)

}
