tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio19 produces correct output", {
  expect_identical(round(bio19(hourly_temps, hourly_prec, tme1, tme1),2), 25693.72)
  expect_identical(round(bio19(hourly_temps, six_hourly_prec, tme1, tme2),2),
                   4287.95)
  expect_identical(round(bio19(hourly_temps, daily_prec, tme1, tme3),2), 1131.79)

  expect_identical(round(bio19(six_hourly_temps, hourly_prec, tme2, tme1),2),
                   25768.15)
  expect_identical(round(bio19(six_hourly_temps, six_hourly_prec, tme2, tme2),2),
                   4280.57)
  expect_identical(round(bio19(six_hourly_temps, daily_prec, tme2, tme3),2),
                   1130.92)
  expect_identical(round(bio19(daily_temps, hourly_prec, tme3, tme1),2), 25939.59)
  expect_identical(round(bio19(daily_temps, six_hourly_prec, tme3, tme2),2),
                   4316.56)
  expect_identical(round(bio19(daily_temps, daily_prec, tme3, tme3),2), 1127.69)
})
