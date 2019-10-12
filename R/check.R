# .check.kind ------------------------------------------------------------------
.check.kind <- function(kind = NULL) 
{  
  msg <- "(Data) kind must be one of \'q\' (water quality), \'h\' (hydraulic) or \'r\' (rain).\n"
  
  if (is.null(kind)) {
    stop("No (data) kind given. ", msg)
  }
  
  if (! kind %in% c("q", "h", "r")) {
    stop(msg)
  }
}

# .check.owner -----------------------------------------------------------------
.check.owner <- function(owner = NULL) 
{  
  msg <- "(Data) owner must be one of \'KWB\', \'SEN\' (Senate) or \'BWB\'.\n"
  
  if (is.null(owner)) {
    stop("No (data) owner given. ", msg)
  }
  
  if (! owner %in% c("KWB", "SEN", "BWB")) {
    stop(msg)
  }
}

# .check.qua.level -------------------------------------------------------------
.check.qua.level <- function(qua.level = NULL) 
{  
  msg <- "Data quality level \'qua.level\' must be one of \'r\' (raw), \'v\' (valid) or \'c\' (calibrated).\n"
  
  if (is.null(qua.level)) {
    stop("No data quality level given. ", msg)
  }
  
  if (! qua.level %in% c("r", "v", "c")) {
    stop(msg)
  }
}

# .check.moniPoint -------------------------------------------------------------
.check.moniPoint <- function(moniPoint = NULL, kind = NULL, owner = NULL) 
{
  .check.kind(kind)
  .check.owner(owner)
  
  # get matrix with short and long names of monitoring points
  mps <- hsMoniPoints(kind, owner = owner)
  
  # string list of monitoring points
  mpl <-  paste("\'", mps[,1], "\' (", mps[,2], ")", sep = "", collapse = "\n  ")
  
  if (is.null(moniPoint)) {
    stop("No monitoring point \'moniPoint\' given. ",
         "Available monitoring points are:\n  ", mpl, "\n")
  }
  
  if (!moniPoint %in% mps[,1]) {
    stop("Monitoring point \'", moniPoint, "\' ",
         "is not in the list of known monitoring points: ", mpl, "\n")
  }
}
