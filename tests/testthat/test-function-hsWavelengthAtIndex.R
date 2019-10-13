test_that("hsWavelengthAtIndex() works", {

  f <- kwb.miacso:::hsWavelengthAtIndex
  
  expect_error(f(), '"i" is missing')
  expect_equal(f(1), 200)
  expect_equal(f(217), 740)
})
