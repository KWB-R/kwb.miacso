test_that("qmdb() works", {

  f <- kwb.miacso:::qmdb
  
  expect_error(f(), "No data quality level given")
  expect_match(f(qua.level = "r", moniPoint = "STA"), "1RAW")
  expect_match(f(qua.level = "v", moniPoint = "STA"), "2VAL")
  expect_match(f(qua.level = "c", moniPoint = "STA"), "3CAL")
})
