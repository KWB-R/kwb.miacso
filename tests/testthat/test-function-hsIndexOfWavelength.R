test_that("hsIndexOfWavelength() works", {

  f <- kwb.miacso:::hsIndexOfWavelength
  
  expect_error(f(), "\"wavelength\" is missing")
  expect_equal(f(200), 1)
  expect_equal(f(740), 217)
  expect_equal(f(750), 221)
  expect_equal(f(750), 221)
  expect_equal(f(732.5), 214)
})
