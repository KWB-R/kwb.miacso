test_that("hsDataSource() works", {

  f <- kwb.miacso:::hsDataSource
  
  expect_error(f(), "No data quality level")
  expect_error(f("r"), "No monitoring point")

  expect_error(f("r", kind = "r"), "rain data not yet implemented")
  expect_error(f("r", kind = "h"), "hydraulic data not yet implemented")
  
  check_structure <- function(result) {
    expect_is(result, "list")
    expect_identical(names(result), c("mdb", "tbl", "tsField", "parField"))
  } 
  
  result <- f(qua.level = "r", moniPoint = "STA", parName = "abc")
  check_structure(result)
  expect_match(result$mdb, "1RAW")
  expect_identical(result$parField, "abc")
  
  result <- f(qua.level = "v", moniPoint = "STA", parName = "abc")
  check_structure(result)
  expect_match(result$mdb, "_VAL")
  expect_identical(result$parField, "abc_A")
  
  result <- f(qua.level = "c", moniPoint = "STA", parName = "abc")
  check_structure(result)
  expect_match(result$mdb, "_CAL")
  expect_identical(result$parField, "abc")
})
