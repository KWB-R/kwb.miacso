test_that(".check.kind() works", {

  f <- kwb.miacso:::.check.kind
  
  expect_error(f())
  expect_error(f("a"))
  expect_silent(f("q"))
  expect_silent(f("h"))
  expect_silent(f("r"))
})

