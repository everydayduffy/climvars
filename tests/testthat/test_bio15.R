tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio15 produces correct output", {
  expect_identical(round(bio15(hourly_prec, tme1),2), 5.54)
  expect_identical(round(bio15(six_hourly_prec, tme2),2), 5.48)
  expect_identical(round(bio15(daily_prec, tme3),2), 5.96)
})
