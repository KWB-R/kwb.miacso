test_that("miadir() works", {
   
  text <- paste(collapse = "\n", c(
    "owner,kind,quaLevel,resol,subdir,      c_owner, c_kind,c_quaLevel,c_resol",
    "KWB,     q,raw,     low,  kawebe,      root_kwb,NA,    NA,        NA",
    "BWB,     q,raw,     low,  beweb,       root_bwb,NA,    NA,        NA",
    "SEN,     q,raw,     low,  senate,      root_sen,NA,    NA,        NA",
    "root_kwb,q,NA,      NA,   /root/to/kwb,NA,      NA,    NA,        NA",
    "root_bwb,q,NA,      NA,   /root/to/bwb,NA,      NA,    NA,        NA",
    "root_sen,q,NA,      NA,   /root/to/sen,NA,      NA,    NA,        NA"
  ))
  
  DS <- read.table(text = text, sep = ",", header = TRUE, na.strings = "")
  DS <- kwb.utils::asNoFactorDataFrame(lapply(DS, function(x) {
    x <- gsub("^\\s*NA\\s*$", NA, x)
    not_na <- ! is.na(x)
    x[not_na] <- gsub("^\\s*(.*)\\s*$", "\\1", x[not_na])
    x
  }))
  DS

  kwb.miacso:::miadir(DS = DS, owner = "KWB")
})
