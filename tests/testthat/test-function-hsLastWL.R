test_that("hsLastWL() works", {

  f <- kwb.miacso:::hsLastWL
  
  expect_error(f(), '"moniPoint" is missing')
  expect_equal(f("MUE"), 732.5)
  expect_equal(f("STA"), 740)
  expect_equal(f("TEG"), 750)
})
