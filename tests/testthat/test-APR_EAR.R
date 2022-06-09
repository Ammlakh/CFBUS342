test_that("APR to EAR Calculator Error", {
  expect_equal(APR_TO_EAR(.1, 12), (1 + .1/12)^12 - 1) #EAR is .1047131
})

test_that("APR to EAR Calculator Error", {
  expect_equal(APR_TO_EAR(.1, 365), (1 + .1/365)^365 - 1) #EAR is .1051558
})

test_that("EAR to APR Calculator Error", {
  expect_equal(EAR_to_APR(.1047131, 12), 12*((1+.1047131)^(1/12)-1)) #APR is .1
})

test_that("EAR to APR Calculator Error", {
  expect_equal(EAR_to_APR(.1051558, 365), 365*((1+.1051558)^(1/365)-1)) #APR is .1
})
