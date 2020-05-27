tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio4 produces correct output", {
  expect_identical(round(bio4(hourly_temps, tme1),2), 5.60)
  expect_identical(round(bio4(six_hourly_temps, tme2),2), 5.44)
  expect_identical(round(bio4(daily_temps, tme3),2), 5.80)
})
