tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio18 produces correct output", {
  expect_identical(round(bio18(hourly_temps, hourly_prec, tme1, tme1),2), 9207.52)
  expect_identical(round(bio18(hourly_temps, six_hourly_prec, tme1, tme2),2),
                   1579.04)
  expect_identical(round(bio18(hourly_temps, daily_prec, tme1, tme3),2), 406.25)

  expect_identical(round(bio18(six_hourly_temps, hourly_prec, tme2, tme1),2),
                   9205.92)
  expect_identical(round(bio18(six_hourly_temps, six_hourly_prec, tme2, tme2),2),
                   1579.36)
  expect_identical(round(bio18(six_hourly_temps, daily_prec, tme2, tme3),2),
                   404.95)
  expect_identical(round(bio18(daily_temps, hourly_prec, tme3, tme1),2), 9313.95)
  expect_identical(round(bio18(daily_temps, six_hourly_prec, tme3, tme2),2),
                   1593.76)
  expect_identical(round(bio18(daily_temps, daily_prec, tme3, tme3),2), 404.95)
})
