tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio14 produces correct output", {
  expect_identical(round(bio14(hourly_precip, tme1),2), 11.8)
  expect_identical(round(bio14(six_hourly_precip, tme2),2), 11.8)
  expect_identical(round(bio14(daily_precip, tme3),2), 11.8)
  expect_identical(round(bio14(hourly_precip, tme1, period = 28),2), 286.41)
  expect_identical(round(bio14(six_hourly_precip, tme2, period = 28),2), 286.41)
  expect_identical(round(bio14(daily_precip, tme3, period = 28),2), 286.41)
})
