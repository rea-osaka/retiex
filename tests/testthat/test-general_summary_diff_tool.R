
test_that("add_change_rate_cols", {

  input <- data.frame(
    date   = c(as.Date("2000-01-01"),
               as.Date("2000-02-01"),
               as.Date("2000-03-01")),
    mean   = c(1,2,3),
    sd     = c(1,2,3),
    min    = c(1,2,3),
    qu1    = c(1,2,3),
    median = c(1,2,3),
    qu3    = c(1,2,3),
    max    = c(1,2,3),
    count  = c(1,2,3)
  )

  output <- data.frame(
    date   = c(as.Date("2000-01-01"),
               as.Date("2000-02-01"),
               as.Date("2000-03-01")),
    mean   = c(1,2,3),
    sd     = c(1,2,3),
    min    = c(1,2,3),
    qu1    = c(1,2,3),
    median = c(1,2,3),
    qu3    = c(1,2,3),
    max    = c(1,2,3),
    count  = c(1,2,3),

    cr_mean   = c(NA, 2.0, 1.5),
    cr_sd     = c(NA, 2.0, 1.5),
    cr_min    = c(NA, 2.0, 1.5),
    cr_qu1    = c(NA, 2.0, 1.5),
    cr_median = c(NA, 2.0, 1.5),
    cr_qu3    = c(NA, 2.0, 1.5),
    cr_max    = c(NA, 2.0, 1.5),
    cr_count  = c(NA, 2.0, 1.5))

  expect_equal(add_change_rate_cols(input), output)
})

test_that("add_change_rate_diff_cols", {

  input <- data.frame(
    date   = c(as.Date("2000-01-01"),
               as.Date("2000-02-01"),
               as.Date("2000-03-01")),
    mean   = c(1,2,3),
    sd     = c(1,2,3),
    min    = c(1,2,3),
    qu1    = c(1,2,3),
    median = c(1,2,3),
    qu3    = c(1,2,3),
    max    = c(1,2,3),
    count  = c(1,2,3)
  )

  output <- data.frame(
    date   = c(as.Date("2000-01-01"),
               as.Date("2000-02-01"),
               as.Date("2000-03-01")),
    mean   = c(1,2,3),
    sd     = c(1,2,3),
    min    = c(1,2,3),
    qu1    = c(1,2,3),
    median = c(1,2,3),
    qu3    = c(1,2,3),
    max    = c(1,2,3),
    count  = c(1,2,3),

    crd_mean   = c(NA, 1.0, 0.5),
    crd_sd     = c(NA, 1.0, 0.5),
    crd_min    = c(NA, 1.0, 0.5),
    crd_qu1    = c(NA, 1.0, 0.5),
    crd_median = c(NA, 1.0, 0.5),
    crd_qu3    = c(NA, 1.0, 0.5),
    crd_max    = c(NA, 1.0, 0.5),
    crd_count  = c(NA, 1.0, 0.5)
  )

  expect_equal(add_change_rate_diff_cols(input), output)
})

test_that("add_change_diff_cols", {

  input <- data.frame(
    date   = c(as.Date("2000-01-01"),
               as.Date("2000-02-01"),
               as.Date("2000-03-01")),
    mean   = c(1,2,3),
    sd     = c(1,2,3),
    min    = c(1,2,3),
    qu1    = c(1,2,3),
    median = c(1,2,3),
    qu3    = c(1,2,3),
    max    = c(1,2,3),
    count  = c(1,2,3)
  )

  output <- data.frame(
    date   = c(as.Date("2000-01-01"),
               as.Date("2000-02-01"),
               as.Date("2000-03-01")),
    mean   = c(1,2,3),
    sd     = c(1,2,3),
    min    = c(1,2,3),
    qu1    = c(1,2,3),
    median = c(1,2,3),
    qu3    = c(1,2,3),
    max    = c(1,2,3),
    count  = c(1,2,3),

    cd_mean   = c(NA, 1, 1),
    cd_sd     = c(NA, 1, 1),
    cd_min    = c(NA, 1, 1),
    cd_qu1    = c(NA, 1, 1),
    cd_median = c(NA, 1, 1),
    cd_qu3    = c(NA, 1, 1),
    cd_max    = c(NA, 1, 1),
    cd_count  = c(NA, 1, 1)

  )

  expect_equal(add_change_diff_cols(input), output)
})

