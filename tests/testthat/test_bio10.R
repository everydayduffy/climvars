tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio10 produces correct output", {
  expect_identical(round(bio10(hourly_temps, tme1),2), 9.74)
  expect_identical(round(bio10(six_hourly_temps, tme2),2), 9.69)
  expect_identical(round(bio10(daily_temps, tme3),2), 9.71)
})

