# hsGetValData -----------------------------------------------------------------

#' Get Validated Data from Validated Database
#'
#' 2011-10-25: created
#' 
#' @param moniPoint name of monitoring point, e.g. "TEG", "STA", "MUE"
#' @param parName parameter name, e.g. "AFS"
#' @param firstDate first date to be selected as "mm/dd/yyyy hh:nn:ss"
#' @param lastDate last date to be selected as "mm/dd/yyyy hh:nn:ss"
#' @return data.frame containing validated data
#' @export
hsGetValData <- function(moniPoint, parName, firstDate, lastDate)
{
  if (missing(moniPoint)) {
    
    msg <- paste(
      "\nUsage: hsGetValData(moniPoint, parName, firstDate, lastDate)",
      "  moniPoint: acronym of monitoring point: \"STA\" (Stallstr.),",
      "\"TEG\" (Tegeler Weg) or \"MUE\" (Muehlendamm)",
      "  parName:     parameter name, e.g. \"AFS\"",
      "  firstDate: first date of requested time period (mm/dd/yyyy)",
      "  lastDate:  last date of requested time period (mm/dd/yyyy)",
      "Returns a data.frame containing the validated data, directly drawn from",
      "the current databases.",
      sep = "\n"
    )
    
    clean_stop(msg)
  }
  
  # Provide path to database, table name and table field names
  src <- hsDataSource("v", moniPoint, parName)
  
  # Create string containing the list of fields to be selected
  fieldList <- sprintf("%s, %s_A, %s_C", src$tsField, parName, parName)
  
  # Validity condition
  condValidity <- sprintf(
    "NOT (%s_A IS NULL AND %s_C IS NULL)", parName, parName
  )
  
  # No condition at all!
  condValidity <- "TRUE"
  
  # Generate SQL string
  sql <- sprintf(
    "SELECT %s FROM %s WHERE %s AND %s BETWEEN #%s# AND #%s#",
    fieldList, src$tbl, condValidity, src$tsField, firstDate, lastDate
  )
  
  print(sql)
  
  ## Run the SQL query
  kwb.db::hsSqlQuery(src$mdb, sql)
}
