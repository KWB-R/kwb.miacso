test_that("hsGetValData() works", {

  f <- kwb.miacso:::hsGetValData
  
  expect_error(f(), "Usage:")
  #f(moniPoint = "STA", parName = "a", "2010-01-01", "2010-12-31")
})
