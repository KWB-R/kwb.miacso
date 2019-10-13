test_that("hsFpFields() works", {

  f <- kwb.miacso:::hsFpFields
  
  expect_error(f(), "\"moniPoint\" is missing, with no default")
  expect_error(f("abc"))
  
  result <- f("STA")
  expect_is(result, "character")
  expect_match(result, "^e200")
  expect_match(result, "e740$")
  expect_length(strsplit(result, ",")[[1]], 217)

  result <- f("MUE")
  expect_is(result, "character")
  expect_match(result, "^e200")
  expect_match(result, "e732.5$")
  expect_length(strsplit(result, ",")[[1]], 214)

  result <- f("TEG")
  expect_is(result, "character")
  expect_match(result, "^e200")
  expect_match(result, "e750$")
  expect_length(strsplit(result, ",")[[1]], 221)
})
