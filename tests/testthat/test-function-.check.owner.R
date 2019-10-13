test_that(".check.owner() works", {

  f <- kwb.miacso:::.check.owner
  
  expect_error(f(), "No \\(data\\) owner")
  expect_error(f("a", "owner must be one of"))
  expect_silent(f("KWB"))
  expect_silent(f("BWB"))
  expect_silent(f("SEN"))
})
