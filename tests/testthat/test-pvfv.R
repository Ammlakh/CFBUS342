test_that("Present Value Calculator Error", {
  expect_equal(PV(2,1,1), 2/((1+1)^1)) #equals $1
})

test_that("Present Value Calculator Error", {
  expect_equal(PV(10,5,.1), 10/((1+.1)^5)) #equals $6.209213
})

test_that("Future Value Calculator Error", {
  expect_equal(FV(1, 1, 1), 1*((1+1)^1)) #equals $2
})

test_that("Future Value Calculator Error", {
  expect_equal(FV(6.209213, 5, .1),  6.209213*((1+.1)^5)) #equals $10
})
