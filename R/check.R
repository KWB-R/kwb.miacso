# .check.kind ------------------------------------------------------------------
.check.kind <- function(kind = NULL) 
{  
  msg <- paste(
    "(Data) kind must be one of 'q' (water quality), 'h' (hydraulic) or", 
    "'r' (rain).\n"
  )
  
  if (is.null(kind)) {
    clean_stop("No (data) kind given. ", msg)
  }
  
  if (! kind %in% c("q", "h", "r")) {
    clean_stop(msg)
  }
}

# .check.owner -----------------------------------------------------------------
.check.owner <- function(owner = NULL) 
{  
  msg <- "(Data) owner must be one of 'KWB', 'SEN' (Senate) or 'BWB'.\n"
  
  if (is.null(owner)) {
    clean_stop("No (data) owner given. ", msg)
  }
  
  if (! owner %in% c("KWB", "SEN", "BWB")) {
    clean_stop(msg)
  }
}

# .check.qua.level -------------------------------------------------------------
.check.qua.level <- function(qua.level = NULL) 
{  
  msg <- paste(
    "Data quality level 'qua.level' must be one of 'r' (raw), 'v' (valid) or",
    "'c' (calibrated).\n"
  )
  
  if (is.null(qua.level)) {
    clean_stop("No data quality level given. ", msg)
  }
  
  if (! qua.level %in% c("r", "v", "c")) {
    clean_stop(msg)
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
  mpl <- paste(sprintf("'%s' (%s)", mps[, 1], mps[, 2]), collapse = "\n  ")

  if (is.null(moniPoint)) {
    clean_stop(
      "No monitoring point 'moniPoint' given. ",
      "Available monitoring points are:\n  ", mpl, "\n"
    )
  }
  
  if (! moniPoint %in% mps[, 1]) {
    clean_stop(
      "Monitoring point '", moniPoint, "' ",
      "is not in the list of known monitoring points: ", mpl, "\n"
    )
  }
}

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}
