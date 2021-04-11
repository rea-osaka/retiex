test_that("style_yen shows humanreadable string", {
  expect_identical(style_yen(123456),"123,456円")
  expect_identical(style_yen(123456,3,"千円"),"123千円")
})
