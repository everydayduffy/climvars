tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio17 produces correct output", {
  expect_identical(round(bio17(hourly_precip, tme1),2), 3339.04)
  expect_identical(round(bio17(six_hourly_precip, tme2),2), 3354.46)
  expect_identical(round(bio17(daily_precip, tme3),2), 3426.79)
})
