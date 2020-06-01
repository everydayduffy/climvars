tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio16 produces correct output", {
  expect_identical(round(bio16(hourly_prec, tme1),2), 25727.59)
  expect_identical(round(bio16(six_hourly_prec, tme2),2), 4292.59)
  expect_identical(round(bio16(daily_prec, tme3),2), 1131.79)
})
