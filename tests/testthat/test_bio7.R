tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio7 produces correct output", {
  expect_identical(round(bio7(hourly_temps, tme1),2), 22.7)
  expect_identical(round(bio7(six_hourly_temps, tme2),2), 22.05)
  expect_identical(round(bio7(daily_temps, tme3),2), 20.48)
})
