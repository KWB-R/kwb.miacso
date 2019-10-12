# hmdb -------------------------------------------------------------------------

#' MS Access Databases (Hydraulic Data)
#' 
#' @param \dots arguments passed to \code{\link{miamdb}}
hmdb <- function(...)
{
  miamdb('h', ...) 
}

# qmdb -------------------------------------------------------------------------

#' MS Access Databases (Water Quality Data) 
#' 
#' @param \dots arguments passed to \code{\link{miamdb}}
qmdb <- function(...)
{
  miamdb('q', ...) 
}

# rmdb -------------------------------------------------------------------------

#' MS Access Databases (Rain Data) 
#' 
#' @param \dots arguments passed to \code{\link{miamdb}}
rmdb <- function(...)
{
  miamdb('r', ...) 
}

# miamdb -----------------------------------------------------------------------

#' Get Path to MS Access Database Used in MIA-CSO
#'
#' @param kind kind of data: "q" = water quality, "h" = hydraulic data, "r" =
#'   rain
#' @param moniPoint name of monitoring point, e.g. "STA", "TEG", "MUE"
#' @param qua.level data quality level ("r" = raw, "v" = validated, "c" =
#'   calibrated)
#' @param owner owner of the data, one of "KWB", "SEN", "BWB"
#' @return This function returns the full path to the Access database containing
#'   the specified kind of data
miamdb <- function(
  kind = NULL, moniPoint = NULL, qua.level = NULL, owner = "KWB"
) 
{  
  .check.kind(kind)           # stop if kind value is invalid
  .check.owner(owner)         # stop if owner value is invalid
  .check.qua.level(qua.level) # stop if qua.level value is invalid
  
  # For KWB own data we need the monitoring point and the data quality level
  # in order to find the correct database
  if (owner == "KWB") {
    .check.moniPoint(moniPoint, kind = kind, owner = owner) # stop if monitoring point is invalid
  }
  
  if (kind == "q") { # water quality data
    if (owner == "KWB") {
      file.path("//moby/miacso$/Daten/ACCESS/KwbMonitoring", 
                switch(EXPR = qua.level, r = sprintf("1RAW/KWB_JoinedData.mdb"),
                       v = sprintf("2VAL/KWB_%s_VAL.mdb", moniPoint),
                       c = sprintf("3CAL/KWB_CAL.mdb")))
    } else if (owner == "SEN") {
      cat("WARNING: no distinction between raw, valid and calibrated data for owner 'SEN'.\n")
      file.path("//moby/miacso$/Daten/ACCESS/Gewaesserguete/kontinuierlich",
                "Messreihen", "Hauptparameter_Senat.mdb")
    } else {
      stop("No water quality data of owner BWB available.\n")      
    }
  } else if (kind == "h") { # hydraulic data
    stop("mdb paths for hydraulic data not yet implemented.\n")
  } else if (kind == "r") {
    stop("mdb paths for rain data not yet implemented.\n")
  }
}

