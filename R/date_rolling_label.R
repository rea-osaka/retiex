#' transform data for quarter rolling analysis
#'
#' make pre data for quarter rolling analysis.
#'
#' @param df data.frame
#' @param nse_date_col name of target date col
#'
#' @return data.frame
#' @export
#'
transform_quarter_rolling_format <- function(df, nse_date_col){

  # TODO
  # 任意の期間でローリングする共通ルーチンを作る
  # 今は四半期決め打ち
  # そもそもローリング（移動平均の「移動」）の
  # 定義や効果等をもう少し精密に勉強すべき

  # `[`や`[[`演算子の引数に列名を用いる場合
  # 文字列ではなくシンボルで受け取る場合に
  # 工夫が必要
  # dplyerの中でないので、!!できない
  date_col_name <- rlang::enquo(nse_date_col)
  date_vector <- df %>%
    dplyr::select(!!date_col_name) %>%
    .[[1]]

  # dfに期間毎変換(今は四半期)した列を追加
  # TODO 期間を任意に出来るように
  df_with_date <- df %>%
    dplyr::mutate(
      classify_date = classify_quarter(date_vector)
    )

  # 最小から最大の連続日付ベクトル
  # 最新から古いものの順に並べる
  # classifyに基づくの連続日付データ（今は四半期）を作る
  # TODO 期間を任意に出来るように
  term <-
    seq_date_by_quarter(min(date_vector),max(date_vector)) %>%
    sort(decreasing = TRUE)

  ans <- NULL

  # 要再考量
  # 期間毎のルーチンを関数化する？
  # 今は、四半期でずらす時期を３、
  # ずらす時期を設定しないと、
  # １周期に満たないクラスが発生する
  # 範囲を対象日付からマイナス１２ヶ月で固定
  for(i in 1:(length(term)-3)){
    tmp_data <- df_with_date %>%
      dplyr::filter(classify_date <= term[[i]],
                    classify_date > term[[i]] - months(12)) %>%
      dplyr::mutate(roll_label = term[[i]])

    ans <- dplyr::bind_rows(ans, tmp_data)
  }

  return(ans)
}
