tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio13 produces correct output", {
  expect_identical(round(bio13(hourly_precip, tme1),2), 1749.11)
  expect_identical(round(bio13(six_hourly_precip, tme2),2), 1749.11)
  expect_identical(round(bio13(daily_precip, tme3),2), 1749.11)
  expect_identical(round(bio13(hourly_precip, tme1, period = 28),2), 3426.87)
  expect_identical(round(bio13(six_hourly_precip, tme2, period = 28),2), 3426.87)
  expect_identical(round(bio13(daily_precip, tme3, period = 28),2), 3426.87)
})
