test_that("calc_individual_volume_rate", {

  input_v <- c(300,300)
  input_road <- c(3,4)
  input_zone <- c("商業地域","第一種住居地域")

  expect_equal(local_calc_individual_volume_rate(input_v, input_road, input_zone), c(2.4,1.6))

  expect_equal(local_calc_individual_volume_rate(400, 6, "商業地域"),3.6)
  expect_equal(local_calc_individual_volume_rate(400, 10, "商業地域"), 4)
  expect_equal(local_calc_individual_volume_rate(200, 4, "第一種住居地域"), 1.6)
  expect_equal(local_calc_individual_volume_rate(200, 4, "第一種住居地域","第一種住居地域"), 2)
  expect_equal(local_calc_individual_volume_rate(300, 4, "近隣商業地域","第一種住居地域"), 2.4)
  expect_equal(local_calc_individual_volume_rate(300, 4, "近隣商業地域"), 2.4)

})

test_that("calc_building_price", {
  #calc_building_price(unit_cost, size, durability, howold)

  expect_equal(local_calc_building_price(250000, 100, c(50,1), c(0,10)),c(25000000,0))

})


test_that("add assumption land unit price", {

  # function(reti_data,
  #          building_unit_cost = 200000,
  #          building_durability = 40,
  #          heighzone = NULL){

  input <- data.frame(
    `取引総額` = c(1.0e+8),
    `容積率` = c(400,500),
    `道路幅員` = c(4,12),
    `都市計画` = c("商業地域"),
    land_size = c(100,1000),
    building_size = c(200,NA),
    howold_building = c(10,10),
    huge_building =c(FALSE,TRUE)
  )

  expect_ans <- data.frame(
    `取引総額` = c(1.0e+8),
    `容積率` = c(400,500),
    `道路幅員` = c(4,12),
    `都市計画` = c("商業地域"),
    land_size = c(100,1000),
    building_size = c(200,NA),
    howold_building = c(10,10),
    huge_building =c(FALSE,TRUE),
    individual_volume_rate = c(2.4, 5),
    assumption_building_size = c(200, 5000),
    assumption_land_unit_price = c((100000000 - ((200 * 200000) * (40 - 10) / 40)) / 100,
                                   (100000000 - ((5000 * 200000) * (40 - 10) / 40)) / 1000))

    real <- retiex_add_assumption_land_unit_price(input)

    expect_equal(real,expect_ans)

})



