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
