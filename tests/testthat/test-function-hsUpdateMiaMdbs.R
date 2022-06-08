test_that("hsUpdateMiaMdbs() works", {

  f <- kwb.miacso:::hsUpdateMiaMdbs
  
  expect_error(f(), '"dfMdbs" is missing')
  #f(dfMdbs = NULL, root = kwb.utils::desktop())
})
