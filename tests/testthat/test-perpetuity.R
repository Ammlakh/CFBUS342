test_that("Perpetuity Present Value Calculator Error", {
  expect_equal(PERPETIUTY_PV(1, .1, .08), 1/(.1-.08)) #equals $50
})

test_that("Perpetuity Present Value Calculator Error", {
  expect_equal(PERPETIUTY_PV(100, .05, .10), 100/(.05-.10)) #equals -$2,000
})

test_that("Perpetuity Interest Rate Calculator Error", {
  expect_equal(PERPETIUTY_R(1, 50, .08), 1/50+.08) #interest_rate = .1
})

test_that("Perpetuity Interest Rate Calculator Error", {
  expect_equal(PERPETIUTY_R(100, -2000, .10),  100/-2000 +.10) #interest_rate = .05
})
