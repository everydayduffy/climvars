tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio1 produces correct output", {
  expect_identical(round(bio1(hourly_temps, tme1),2), 6.81)
  expect_identical(round(bio1(six_hourly_temps, tme2),2), 6.81)
  expect_identical(round(bio1(daily_temps, tme3),2), 6.81)
  expect_identical(round(bio1(hourly_temps, tme1, method = "dailymaxmin"),2), 6.75)
  expect_identical(round(bio1(six_hourly_temps, tme2, method = "dailymaxmin"),2), 6.81)
  expect_identical(round(bio1(daily_temps, tme3, method = "dailymaxmin"),2), 6.81)
})
