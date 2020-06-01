tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio12 produces correct output", {
  expect_identical(round(bio12(hourly_prec, tme1),2), 65344.76)
  expect_identical(round(bio12(six_hourly_prec, tme2),2), 10927.82)
  expect_identical(round(bio12(daily_prec, tme3),2), 2841.83)
})
