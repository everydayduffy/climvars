tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio14 produces correct output", {
  expect_identical(round(bio14(hourly_prec, tme1),2), 650.38)
  expect_identical(round(bio14(six_hourly_prec, tme2),2), 105.16)
  expect_identical(round(bio14(daily_prec, tme3),2), 26.12)
  expect_identical(round(bio14(hourly_prec, tme1, period = 28),2), 2671.64)
  expect_identical(round(bio14(six_hourly_prec, tme2, period = 28),2), 451.76)
  expect_identical(round(bio14(daily_prec, tme3, period = 28),2), 114.35)
})
