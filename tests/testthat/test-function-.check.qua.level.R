test_that(".check.qua.level() works", {

  f <- kwb.miacso:::.check.qua.level
  
  expect_error(f(), "No data quality level")
  expect_error(f("a"))
  expect_silent(f("r"))
  expect_silent(f("v"))
  expect_silent(f("c"))
})
