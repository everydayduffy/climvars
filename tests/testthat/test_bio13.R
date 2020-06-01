tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio13 produces correct output", {
  expect_identical(round(bio13(hourly_prec, tme1),2), 2315.88)
  expect_identical(round(bio13(six_hourly_prec, tme2),2), 381.62)
  expect_identical(round(bio13(daily_prec, tme3),2), 100.74)
  expect_identical(round(bio13(hourly_prec, tme1, period = 28),2), 8836.82)
  expect_identical(round(bio13(six_hourly_prec, tme2, period = 28),2), 1481.68)
  expect_identical(round(bio13(daily_prec, tme3, period = 28),2), 383.75)
})
