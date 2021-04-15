#' get summary by year
#'
#' get summary of a col which you like by year.
#'
#' @param df data.frame
#' @param nse_summary_col target col which you like to get summary
#' @param nse_date_col date col
#'
#' @return data.frame
#' @export
#'
get_summary_by_year <- function(df, nse_summary_col, nse_date_col){

  target_summary_col <- rlang::enquo(nse_summary_col)
  date_col <- rlang::enquo(nse_date_col)

  # local_get_summary_by_general_term側は
  # nse_summary_colとnse_date_colを受け取って
  # rlang::enquoするので、
  # 渡すときに!!出来る

  ans <-
    local_get_summary_by_general_term(
      df,
      nse_summary_col =  !!target_summary_col,
      nse_date_col =  !!date_col,
      classify_func = classify_year,
      sequence_func = seq_date_by_year )

  return(ans)
}

#' get summary by quarter
#'
#' get summary of a col which you like by quarter.
#'
#' @param df data.frame
#' @param nse_summary_col target col which you like to get summary
#' @param nse_date_col date col
#'
#' @return data.frame
#' @export
#'
get_summary_by_quarter <- function(df, nse_summary_col, nse_date_col){

  target_summary_col <- rlang::enquo(nse_summary_col)
  date_col <- rlang::enquo(nse_date_col)

  # local_get_summary_by_general_term側は
  # nse_summary_colとnse_date_colを受け取って
  # rlang::enquoするので、
  # 渡すときに!!出来る

  ans <-
    local_get_summary_by_general_term(
      df,
      nse_summary_col =  !!target_summary_col,
      nse_date_col =  !!date_col,
      classify_func = classify_quarter,
      sequence_func = seq_date_by_quarter )

  return(ans)
}

#' get summary by month
#'
#' get summary of a col which you like by month.
#'
#' @param df data.frame
#' @param nse_summary_col target col which you like to get summary
#' @param nse_date_col date col
#'
#' @return data.frame
#' @export
#'
get_summary_by_month <- function(df, nse_summary_col, nse_date_col){

  target_summary_col <- rlang::enquo(nse_summary_col)
  date_col <- rlang::enquo(nse_date_col)

  # local_get_summary_by_general_term側は
  # nse_summary_colとnse_date_colを受け取って
  # rlang::enquoするので、
  # 渡すときに!!出来る

  ans <-
    local_get_summary_by_general_term(
      df,
      nse_summary_col =  !!target_summary_col,
      nse_date_col =  !!date_col,
      classify_func = classify_month,
      sequence_func = seq_date_by_month )

  return(ans)
}

#' get rolling quarter summary
#'
#' get summary data by rolling quarter term.
#'
#' @param df data.frame
#' @param nse_summary_col terget col name which you like to know
#' @param nse_date_col terget date col name
#'
#' @return data.frame
#' @export
#'
get_summary_by_rolling_quarter <-
  function(df, nse_summary_col, nse_date_col){

  target_summary_col <- rlang::enquo(nse_summary_col)
  date_col <- rlang::enquo(nse_date_col)

  # local_get_summary_by_general_term側は
  # nse_summary_colとnse_date_colを受け取って
  # rlang::enquoするので、
  # 渡すときに!!出来る

  df_r <- local_rolling_tranceform(df, !!date_col)

  ans <-
    local_get_summary_by_general_term(
      df_r,
      nse_summary_col =  !!target_summary_col,
      nse_date_col =  roll_label,
      classify_func = classify_quarter,
      sequence_func = seq_date_by_quarter ) %>%
    local_add_diff_cols()

  return(ans)
}



#######################################################################
# ローカル関数
#######################################################################

# 四半期のみ
# TODO 月毎のものもつくる
local_rolling_tranceform <- function(df, nse_date_col){


  # `[`や`[[`演算子の引数に列名を用いる場合
  # 文字列ではなくシンボルで受け取る場合に
  # 工夫が必要
  # dplyerの中でないので、!!できない
  date_col_name <- rlang::enquo(nse_date_col)
  date_vector <- df %>%
    dplyr::select(!!date_col_name) %>%
    .[[1]]

  # 四半期期首日付に変換した日付列を追加
  df_with_date <- df %>%
    dplyr::mutate(
      date = classify_quarter(date_vector)
    )

  # 最小から最大の連続日付ベクトル
  # 最新から古いものの順に並べる
  term <-
    seq_date_by_quarter(min(date_vector),max(date_vector)) %>%
    sort(decreasing = TRUE)

  ans <- NULL

  for(i in 1:(length(term)-3)){
    tmp_data <- df_with_date %>%
      dplyr::filter(date <= term[[i]],
                    date > term[[i]] - months(12)) %>%
      dplyr::mutate(roll_label = term[[i]])

    ans <- dplyr::bind_rows(ans, tmp_data)
  }

  return(ans)
}


# ローカル関数
# 期間毎のサマリーを作成するための
# 共通関数
local_get_summary_by_general_term <-
  function(df,
           nse_summary_col,
           nse_date_col,
           classify_func,
           sequence_func){

    # `[`や`[[`演算子の引数に列名を用いる場合
    # 文字列ではなくシンボルで受け取る場合に
    # 工夫が必要
    # dplyerの中でないので、!!できない
    date_col_name <- rlang::enquo(nse_date_col)
    date_vector <- df %>%
      dplyr::select(!!date_col_name) %>%
      .[[1]]

    tmp_data <-
      df %>%
      dplyr::mutate(date = classify_func(date_vector))

    # サマリー対象の列名は、dplyerの引数に入れるのでenquoして
    # dplyerの引数の中で!!する

    target_col_name <-
      rlang::enquo(nse_summary_col)

    ans <-
      tmp_data %>%
      get_summary_by_group(!!target_col_name, date)


    #################################################
    # 期間の連続性を確認する処理
    # 受け取ったseq_funcで連続日付データを作成して確認
    #################################################

    rg <- range(ans$date)

    ts <- sequence_func(min(ans$date),max(ans$date))

    # 最大最小の範囲内で欠けた日付を探す
    luck_date <- ts[!(ts %in% ans$date)]

    # 欠けた日付があった場合の処理
    if(length(luck_date) != 0){

      for(i in seq_along(luck_date)){

        ans <-
          local_add_row_filled_data(ans, luck_date[[i]])

      }

      ans <- ans %>% dplyr::arrange(date)

    }

    return(ans)
  }


# ローカル関数
# 任意のdfの列を任意のデータ(デフォルト0)で埋める
# date列に与えられた日付を入れる
local_add_row_filled_data <-
  function(df, d, filldata = as.integer(0)){

    tmp_row <- df[1,]

    for(j in 1:ncol(tmp_row)){
      tmp_row[,j] <- filldata
    }

    tmp_row$date <- d

    ans <- dplyr::bind_rows(df, tmp_row)

    return(ans)
  }
