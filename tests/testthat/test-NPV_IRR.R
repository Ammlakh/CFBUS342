test_that("Net Present Value Calculator Error", {
  expect_equal(NPV(c(0, 1, 2, 3, 0), .1), 4.8159279) #NPV = $4.8159279
})

test_that("Net Present Value Calculator Error", {
  expect_equal(NPV(c(0, 5, 10, 15, 0), .1), 24.07963937) #NPV = $24.07963937
})

test_that("Present Value Calculator Error", {
  expect_equal(IRR(c(1, 1.1)), .1) #equals .1
})

test_that("Present Value Calculator Error", {
  expect_equal(IRR(c(10, 0, 10, 10)), 0.3247) #equals 0.3247
})
