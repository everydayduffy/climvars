tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio19 produces correct output", {
  expect_identical(round(bio19(hourly_temps, hourly_precip, tme1, tme1),2),
                   5481.94)
  expect_identical(round(bio19(hourly_temps, six_hourly_precip, tme1, tme2),2),
                   5519.67)
  expect_identical(round(bio19(hourly_temps, daily_precip, tme1, tme3),2),
                   5521.38)
  expect_identical(round(bio19(six_hourly_temps, hourly_precip, tme2, tme1),2),
                   5519.67)
  expect_identical(round(bio19(six_hourly_temps, six_hourly_precip, tme2, tme2),
                         2), 5519.67)
  expect_identical(round(bio19(six_hourly_temps, daily_precip, tme2, tme3),2),
                   5521.38)
  expect_identical(round(bio19(daily_temps, hourly_precip, tme3, tme1),2),
                   5521.38)
  expect_identical(round(bio19(daily_temps, six_hourly_precip, tme3, tme2),2),
                   5521.38)
  expect_identical(round(bio19(daily_temps, daily_precip, tme3, tme3),2),
                   5521.38)
})
