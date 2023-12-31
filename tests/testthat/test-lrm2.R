test_that("lrm works", {
  m1 = lrm(mtcars$mpg, mtcars$wt)
  expect_equal(round(m1[,1],3),
               round(lm(mpg~wt, data=mtcars)$coefficients,3), ignore_attr = TRUE)

  m2 = lrm(CO2$uptake, CO2$conc)
  expect_equal(round(m2[,1],3),
               round(lm(uptake~conc, data=CO2)$coefficients,3), ignore_attr = TRUE)
})
