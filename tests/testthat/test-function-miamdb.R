test_that("miamdb() works", {

  f <- kwb.miacso:::miamdb
  
  expect_error(f(), "No \\(data\\) kind given")
  
  expect_match(f(kind = "q", qua.level = "r", moniPoint = "STA"), "1RAW")
  expect_match(f(kind = "q", qua.level = "v", moniPoint = "STA"), "2VAL")
  expect_match(f(kind = "q", qua.level = "c", moniPoint = "STA"), "3CAL")
})
