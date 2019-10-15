# hsGetFpAndValRaw -------------------------------------------------------------

#' Read Values and Fingerprints from Database
#' 
#' For the given parameters, the values and fingerprints are read from the
#' database.
#' 
#' @param moniPoint name of monitoring point
#' @param parNames vector of parameter names
#' @param year year of which data is requested
#' @param firstDate first date of requested time period
#' @param lastDate last date of requested time period
#' @param home if \code{TRUE}, Hauke's home path is used instead of kwb office
#'   path
#' @return Return the query result as a new data.frame (which is a special type
#'   of a list) with elements "dts" (date-times), "pars" (parameter values) and
#'   "fps" (fingerprints)
#' @export
hsGetFpAndValRaw <- function(
  moniPoint, parNames, year, firstDate, lastDate, home = FALSE
) 
{
  if (missing(moniPoint)) {
    
    msg <- paste(
      "\nUsage: hsGetFpAndValRaw(moniPoint, parNames, year, firstDate,",
      "lastDate)",
      "  moniPoint: acronym of monitoring point: \"STA\" (Stallstr.),", 
      "\"TEG\" (Tegeler Weg) or \"MUE\" (Muehlendamm)",
      "  parNames: vector of parameter names, e.g. c(\"AFS\", \"CSB\")",
      "  year: year of which data is requested",
      "  firstDate: first date of requested time period (mm/dd/yyyy)",
      "  lastDate: last date of requested time period (mm/dd/yyyy)",
      "Returns a data.frame with elements:",
      "  \"dts\":  timestamps,",
      "  \"pars\": parameter values and",
      "  \"fps\":  fingerprints",
      sep = "\n"
    )
    
    clean_stop(msg)
  }
  
  # Determine database name
  mdb <- sprintf("KWB_%s_Scan_Joined.mdb", moniPoint)
  
  # Determine table name
  tbl <- paste("tbl_KWB_", moniPoint, "_ScanFP_Par_", year, sep="")
  
  mdbFullPath <- ifelse(
    home,
    file.path("D:/_Hauke/tmp", mdb),
    file.path(
      "C:/Users/hsonne/_EigeneDateien/_Projekte_lokal/_Projekt_MIA_CSO_lokal", 
      "DbDevelopment/DbData/RAW_Scan", mdb
    )
  )
  
  # Create string containing the list of fields to be selected
  fields <- gsub(".", "_", hsFpFields(moniPoint), fixed = TRUE)
  pars <- paste(parNames, collapse = ",")
  
  # Generate SQL string
  sql <- sprintf(
    "SELECT myDateTime, %s, %s FROM %s WHERE %s AND %s",
    pars, fields, tbl,
    # validity condition
    sprintf("NOT (%s)", kwb.db::hsSqlExOr(parNames, "IsNull")), 
    kwb.db:::hsSqlExTimeCond(
      "myDateTime", dateFirst = firstDate, dateLast = lastDate
    ) # Time interval condition
  )
  
  # Run the SQL query
  res <- kwb.db::hsSqlQuery(mdb, sql)
  
  # Return the query result as a matrix
  #data.matrix(res)
  
  # Return the query result as a new data.frame (which is a special type of a
  # list) with elements "dts" (date-times), "pars" (parameter values) and
  # "fps" (fingerprints)
  i <- 1 + length(pars)
  
  list(
    dts = res[, 1], 
    pars = as.data.frame(res[pars]),
    fps = as.matrix(res[, (i + 1):ncol(res)])
  )
}

# hsFpFields -------------------------------------------------------------------

#' Names of Fields in Fingerprint Table
#' 
#' String representing comma separated list of fields in fingerprint table
#' 
#' @param moniPoint acronym of monitoring point: "STA", "TEG" or "MUE"
#' @return String representing comma separated list of fields in fingerprint
#'   table
#' @export
hsFpFields <- function(moniPoint)
{
  sprintf("e%s", paste(seq(200, hsLastWL(moniPoint), 2.5), collapse = ",e"))
}
