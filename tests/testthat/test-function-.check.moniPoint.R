test_that(".check.moniPoint() works", {

  f <- kwb.miacso:::.check.moniPoint
  
  expect_error(f(), "No \\(data\\) kind")
  expect_error(f(kind = "q", "No \\(data\\) owner"))
  expect_error(f(kind = "q", owner = "a"), "owner must be one of")
  expect_error(f(kind = "q", owner = "KWB"), "No monitoring point")
  expect_error(f("Stallstr", "q", "KWB"), "not in the list of known monitoring")
})
