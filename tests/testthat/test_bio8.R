tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio8 produces correct output", {
  expect_identical(round(bio8(hourly_temps, hourly_prec, tme1, tme1),2), 2.11)
  expect_identical(round(bio8(hourly_temps, six_hourly_prec, tme1, tme2),2),
                   2.1)
  expect_identical(round(bio8(hourly_temps, daily_prec, tme1, tme3),2), 2.11)

  #expect_identical(round(bio8(six_hourly_temps, hourly_prec, tme2, tme1),2), 2.11)
  expect_identical(round(bio8(six_hourly_temps, six_hourly_prec, tme2, tme2),2), 2.14)
  expect_identical(round(bio8(six_hourly_temps, daily_prec, tme2, tme3),2), 2.14)

  #expect_identical(round(bio8(daily_temps, hourly_prec, tme3, tme1),2), 2.11)
  #expect_identical(round(bio8(daily_temps, six_hourly_prec, tme3, tme2),2), 2.11)
  expect_identical(round(bio8(daily_temps, daily_prec, tme3, tme3),2), 1.83)

})

