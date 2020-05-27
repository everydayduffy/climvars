tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio5 produces correct output", {
  expect_identical(round(bio5(hourly_temps, tme1),2), 11.52)
  expect_identical(round(bio5(six_hourly_temps, tme2),2), 11.62)
  expect_identical(round(bio5(daily_temps, tme3),2), 11.43)
})
