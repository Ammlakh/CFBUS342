test_that("Big Ugly Present Value Calculator Error", {
  expect_equal(BIG_UGLY_PV(1000, 30, .06, .03), (1000/(.06-.03))*(1-((1+.03)/(1+.06))^30)) #PV = 19246.3
})

test_that("Big Ugly Present Value Calculator Error", {
  expect_equal(BIG_UGLY_PV(1000, 30, .06, .03, annuity = T), ((1000/(.06-.03))*(1-((1+.03)/(1+.06))^30))*(1+.06)) #PV = 20401.08
})

test_that("Big Ugly Payment Calculator Error", {
  expect_equal(BIG_UGLY_PAYMENT(19246.3, 30, .06, .03), (19246.3*(.06-.03))/(1-((1+.03)/(1+.06))^30)) #Payment = 1,000
})

test_that("Big Ugly Payment Calculator Error", {
  expect_equal(BIG_UGLY_PAYMENT(20401.08, 30, .06, .03, annuity = T), (20401.08*(.06-.03))/(1-((1+.03)/(1+.06))^30)/(1+.06))  #Payment = 1000
})
