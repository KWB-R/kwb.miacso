test_that("rmdb() works", {

  f <- kwb.miacso:::rmdb
  
  expect_error(f(), "No data quality level given")
  expect_error(f(qua.level = "r"), "not yet implemented")
})
