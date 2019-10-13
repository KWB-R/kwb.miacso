test_that("hsReadMiaMdbs() works", {

  f <- kwb.miacso:::hsReadMiaMdbs
  
  expect_error(f())
})
