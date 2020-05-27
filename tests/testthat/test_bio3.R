tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio3 produces correct output", {
  expect_identical(round(bio3(hourly_temps, tme1),2), 0.30)
  expect_identical(round(bio3(six_hourly_temps, tme2),2), 0.16)
  expect_identical(bio3(daily_temps, tme3), 0)
})
