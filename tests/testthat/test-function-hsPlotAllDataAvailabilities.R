test_that("hsPlotAllDataAvailabilities() works", {

  f <- kwb.miacso:::hsPlotAllDataAvailabilities
  
  expect_error(f(), '"moniPoints" is missing')
  expect_error(f(moniPoints = "x"), '"pdfDir" is missing')
  expect_error(f("x", pdfDir = tempdir()), '"dates" is missing')
  
  expect_error(
    f("STA", pdfDir = tempdir(), dates = c("2010-07-01", "2010-08-01"), 
    parNames = "a"), 
    "setCurrentSqlDialect"
  )
})
