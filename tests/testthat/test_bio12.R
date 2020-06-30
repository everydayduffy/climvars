tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio12 produces correct output", {
  expect_identical(round(bio12(hourly_precip, tme1),2), 21709.56)
  expect_identical(round(bio12(six_hourly_precip, tme2),2), 21709.56)
  expect_identical(round(bio12(daily_precip, tme3),2), 21709.56)
})
