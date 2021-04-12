test_that("get_summary_by_group make each summary by group", {

  # 入力
  d1 <- data.frame(g = "a", v = 1:10)
  d2 <- data.frame(g = "b", v = 1:15)
  d3 <- data.frame(g = "c", v = 100:200)

  input <- dplyr::bind_rows(d1,d2,d3)

  # 正しい出力を別の方法で準備
  right_res <- dplyr::bind_rows(
    d1 %>% dplyr::group_by(g) %>% get_summary(v),
    d2 %>% dplyr::group_by(g) %>% get_summary(v),
    d3 %>% dplyr::group_by(g) %>% get_summary(v)
  )

  expect_identical(get_summary_by_group(input, v, g), right_res)
  })
