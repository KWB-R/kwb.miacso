test_that("hsPlotMiaCsoAvailabilities() works", {

  f <- kwb.miacso:::hsPlotMiaCsoAvailabilities
  
  expect_error(f(), '"parName" is missing')
  expect_error(f(parName = "a"), '"moniPoint" is missing')
  expect_error(f(parName = "a", moniPoint = "b"), '"dateFirst" is missing')
  
  expect_error(
    f(parName = "a", moniPoint = "b", dateFirst = "2010-07-15", 
      dateLast = "2011-07-15"), 
    '"qTypes" is missing'
  )

  expect_error(
    f(parName = "a", moniPoint = "STA", dateFirst = "2010-07-15", 
      dateLast = "2011-07-15", qTypes = c("r", "v")),
    "use setCurrentSqlDialect"
  )
})
