test_that("clean_stop() works", {

  f <- kwb.miacso:::clean_stop
  
  expect_error(f("abc"), "abc")
})
