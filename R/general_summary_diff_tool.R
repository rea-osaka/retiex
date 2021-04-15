#' add change rate cols
#'
#' add cols which was mutated as change rate from past term data
#' (lag target col)  into the summary data
#'
#' @param summary_df
#'
#' @return data.frame
#' @export
#'
add_change_rate_cols <- function(summary_df){

  ans <- summary_df %>%
    dplyr::mutate(
      # 対前期間比(増減)
      cr_mean   = mean   / dplyr::lag(mean),
      cr_sd     = sd     / dplyr::lag(sd),
      cr_min    = min    / dplyr::lag(min),
      cr_qu1    = qu1    / dplyr::lag(qu1),
      cr_median = median / dplyr::lag(median),
      cr_qu3    = qu3    / dplyr::lag(qu3),
      cr_max    = max    / dplyr::lag(max),
      cr_count  = count  / dplyr::lag(count))

  return(ans)
}

#' add change rate cols
#'
#' add cols which was mutated as change rate diff from past term data
#' (lag target col)  into the summary data
#'
#' @param summary_df
#'
#' @return data.frame
#' @export
#'
add_change_rate_diff_cols <- function(summary_df){

  ans <- summary_df %>%
    dplyr::mutate(
      # 対前期間比(増減)
      crd_mean   = mean   / dplyr::lag(mean) - 1,
      crd_sd     = sd     / dplyr::lag(sd) - 1,
      crd_min    = min    / dplyr::lag(min) -1,
      crd_qu1    = qu1    / dplyr::lag(qu1) -1,
      crd_median = median / dplyr::lag(median) -1,
      crd_qu3    = qu3    / dplyr::lag(qu3) -1,
      crd_max    = max    / dplyr::lag(max) -1,
      crd_count  = count  / dplyr::lag(count) -1)

  return(ans)
}

#' add change diff cols
#'
#' add cols which was mutated as change diff from past term data
#' (lag target col)  into the summary data
#'
#' @param summary_df data.frame
#'
#' @return data.frame
#' @export
#'
add_change_diff_cols <- function(summary_df){

  ans <- summary_df %>%
    dplyr::mutate(
      # 対前期間差(増減)
      cd_mean   = mean   - dplyr::lag(mean),
      cd_sd     = sd     - dplyr::lag(sd),
      cd_min    = min    - dplyr::lag(min),
      cd_qu1    = qu1    - dplyr::lag(qu1),
      cd_median = median - dplyr::lag(median),
      cd_qu3    = qu3    - dplyr::lag(qu3),
      cd_max    = max    - dplyr::lag(max),
      cd_count  = count  - dplyr::lag(count))

    return(ans)
}

