tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio16 produces correct output", {
  expect_identical(round(bio16(hourly_precip, tme1),2), 7561.89)
  expect_identical(round(bio16(six_hourly_precip, tme2),2), 7572.68)
  expect_identical(round(bio16(daily_precip, tme3),2), 7595.87)
})
