tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio2 produces correct output", {
  expect_identical(round(bio2(hourly_temps, tme1),2), 3.65)
  expect_identical(round(bio2(six_hourly_temps, tme2),2), 1.91)
  expect_identical(bio2(daily_temps, tme3), 0)
})
