test_that("hmdb() works", {

  f <- kwb.miacso:::hmdb
  
  expect_error(f(), "No data quality level given")
  expect_error(f(qua.level = "r"), "not yet implemented")
  expect_error(f(qua.level = "v"), "not yet implemented")
  expect_error(f(qua.level = "c"), "not yet implemented")
})
