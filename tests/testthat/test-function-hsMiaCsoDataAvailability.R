test_that("hsMiaCsoDataAvailability() works", {

  f <- kwb.miacso:::hsMiaCsoDataAvailability
  
  expect_error(f(), '"level" is missing')
  expect_error(f(level = "r"), '"moniPoint" is missing')
  expect_error(f(level = "r", moniPoint = "a"), "not in the list of known moni")
  expect_error(f(level = "r", moniPoint = "STA"), '"parName" is missing')
  #f(level = "r", moniPoint = "STA", parName = "a")
})
