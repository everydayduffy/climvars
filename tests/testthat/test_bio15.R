tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio15 produces correct output", {
  expect_identical(round(bio15(hourly_precip, tme1),2), 1.93)
  expect_identical(round(bio15(six_hourly_precip, tme2),2), 11.56)
  expect_identical(round(bio15(daily_precip, tme3),2), 46.25)
})
