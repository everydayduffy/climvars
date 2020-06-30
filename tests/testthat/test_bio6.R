tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio6 produces correct output", {
  expect_identical(round(bio6(hourly_temps, tme1),2), -3.68)
  expect_identical(round(bio6(six_hourly_temps, tme2),2), -3.37)
  expect_identical(round(bio6(daily_temps, tme3),2), -2.72)
})
