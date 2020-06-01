tme1 <- tmecreate(2010, 1)
tme2 <- tmecreate(2010, 6)
tme3 <- tmecreate(2010, 24)

test_that("bio17 produces correct output", {
  expect_identical(round(bio17(hourly_prec, tme1),2), 9176.56)
  expect_identical(round(bio17(six_hourly_prec, tme2),2), 1570.26)
  expect_identical(round(bio17(daily_prec, tme3),2), 404.95)
})
