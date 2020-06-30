tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio11 produces correct output", {
  expect_identical(round(bio11(hourly_temps, tme1),2), 1.86)
  expect_identical(round(bio11(six_hourly_temps, tme2),2), 1.85)
  expect_identical(round(bio11(daily_temps, tme3),2), 1.85)
})
