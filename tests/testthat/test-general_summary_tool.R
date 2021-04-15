test_that("get_summary test", {
  # 入力
  input <- data.frame(target = c(1,2,3,4,5,6,7,8,9,-1,-100))

  # 正しい出力を別の方法で準備
  # 列名の確認
  right_res <-
    data.frame(
      mean = mean(input$target),
      sd = stats::sd(input$target),
      min = min(input$target),
      qu1 = as.numeric(quantile(input$target)[2]),
      median = median(input$target),
      qu3 = as.numeric(quantile(input$target)[4]),
      max = max(input$target),
      count = length(input$target)
    )

  expect_identical(get_summary(input, target),right_res)
})

test_that("get_summary_by_group test", {

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
