test_that("get_summary make summary data.frame", {
  # 入力
  input <- data.frame(target = c(1,2,3,4,5,6,7,8,9,-1,-100))

  # 正しい出力を別の方法で準備
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
