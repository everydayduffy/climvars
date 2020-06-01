tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio9 produces correct output", {
  expect_identical(round(bio9(hourly_temps, hourly_prec, tme1, tme1),2), 9.73)
  expect_identical(round(bio9(hourly_temps, six_hourly_prec, tme1, tme2),2),
                   9.72)
  expect_identical(round(bio9(hourly_temps, daily_prec, tme1, tme3),2), 9.73)
  expect_identical(round(bio9(six_hourly_temps, hourly_prec, tme2, tme1),2),
                   9.68)
  expect_identical(round(bio9(six_hourly_temps, six_hourly_prec, tme2, tme2),2),
                   9.66)
  expect_identical(round(bio9(six_hourly_temps, daily_prec, tme2, tme3),2),
                   9.67)
  expect_identical(round(bio9(daily_temps, hourly_prec, tme3, tme1),2), 9.66)
  expect_identical(round(bio9(daily_temps, six_hourly_prec, tme3, tme2),2),
                   9.65)
  expect_identical(round(bio9(daily_temps, daily_prec, tme3, tme3),2), 9.71)
})
