test_that("hsGetFpAndValRaw() works", {

  f <- kwb.miacso:::hsGetFpAndValRaw
  
  expect_error(f(), "Usage:")
  expect_error(f("STA"), '"year" is missing')
  expect_error(f("STA", year = 2010), '"parNames" is missing')
  #f("STA", year = 2010, parNames = c("a", "b"))
})
