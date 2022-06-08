test_that("hsPars() works", {

  f <- kwb.miacso:::hsPars
  
  expect_error(f(), "No \\(data\\) kind given")
  expect_error(f(kind = "q", moniPoint = "STA"), "not yet implemented")
  #f(kind = "q", moniPoint = "STA", qua.level = "r")
})
