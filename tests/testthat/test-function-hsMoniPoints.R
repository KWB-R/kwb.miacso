test_that("hsMoniPoints() works", {

  f <- kwb.miacso:::hsMoniPoints
  
  expect_error(f(), "No \\(data\\) kind given")
  expect_error(f(kind = "r"), "rain data not yet implemented")
  expect_error(f(kind = "h"), "hydraulic data not yet implemented")
  
  result <- f(kind = "q")
  expect_is(result, "matrix")
  expect_identical(result[, 1], c("STA", "MUE", "TEG"))
})
