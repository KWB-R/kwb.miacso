# hsDataSource -----------------------------------------------------------------

#' Get Metadata About Data Sources 
#' 
#' Returns a list containing \enumerate{
#'   \item full path to Access database,
#'   \item table name,
#'   \item name of timestamp field, 
#'   \item name of parameter field
#' } 
#' for the table that contains data of parameter \code{parName}, measured at
#' monitoring point \code{moniPoint} in data quality level \code{qua.level}.
#' 
#' @param qua.level data quality level ("r" = raw, "v" = validated, "c" =
#'   calibrated)
#' @param moniPoint name of monitoring point, e.g. "STA", "TEG", "MUE"
#' @param parName name of parameter, e.g. "AFS", "CSB", "CSBf"
#' @param kind kind of data: "q" = water quality, "h" = hydraulic data, "r" =
#'   rain
#' @param owner owner of the data, one of "KWB", "SEN", "BWB"
#' @param dbg whether to show debug messages or not
#' @return Returns a list with the following named elements: \enumerate{
#'   \item \code{mdb}: full path to Access database, 
#'   \item \code{tbl}: table name,
#'   \item \code{tsField}: name of timestamp field, 
#'   \item \code{parField}: name of parameter field
#' }
#' @export
#' @examples 
#' # Get source information of validated data of parameter AFS from Stallstr. 
#' si <- hsDataSource("v", "STA", "AFS")
#' si 
#' # ouput:
#' #  $db
#' #  [1] "//moby/miacso$/Daten/ACCESS/KwbMonitoring/2VAL/KWB_STA_VAL.mdb"
#' # 
#' #  $tbl
#' #  [1] "KWB_STA_ScanPar_AFS_VAL"
#' # 
#' #  $tsfield
#' #  [1] "myDateTime"
#' # 
#' #  $parfield
#' #  [1] "AFS_A"
#' 
#' # Access the pieces of information with the $ operator:
#' si$mdb # [1] "//moby/miacso$/Daten/ACCESS/KwbMonitoring/2VAL/KWB_STA_VAL.mdb"
#' si$tbl # [1] "KWB_STA_ScanPar_AFS_VAL"
hsDataSource <- function(
  qua.level = NULL, moniPoint = NULL, parName, kind = "q", owner = "KWB",  
  dbg = FALSE
) 
{
  basePath <- kwbMonitoringPath()
  
  tbl <- ""
  tsField <- ""
  parField <- ""
  
  #  if (missing(qua.level)) {
  #    msg <- paste("\nUsage: hsDataSource(qua.level, moniPoint, parName)",
  #      "  qua.level: \"r\" (raw), \"v\" (valid) or \"c\" (calibrated)",
  #      "  moniPoint: acronym of monitoring point: \"STA\" (Stallstr.), \"TEG\" (Tegeler Weg) or \"MUE\" (Muehlendamm)",
  #      "  parName: acronym of measured parameter, e.g. \"AFS\", \"CSB\", \"CSBf\", ...",
  #      "Returns a data.frame with four fields:",
  #      "  1. mdb:      full path to Access database,",
  #      "  2. tbl:      table name,",
  #      "  3. tsField:  name of timestamp field in table,",
  #      "  4. parField: name of parameter field in table.",
  #      sep = "\n")
  #    clean_stop(msg)
  #  }
  
  .check.owner(owner)
  .check.kind(kind) 
  .check.qua.level(qua.level)
  .check.moniPoint(moniPoint, kind = kind, owner = owner)
  
  ## provide meta data in data frame "info"
  info <- data.frame(
    kind = c("q", "q", "q"),
    owner = c("KWB", "KWB", "KWB"),
    qua.level = c("r", "v", "c"),
    tblPtrn = c(
      "qry_KWB_%s_ScanPar",    # raw
      "KWB_%s_ScanPar_%s_VAL", # validated
      "qry_%s_CAL_%s"          # calibrated
    ),
    tsField = c("myDateTime", "myDateTime", "myDateTime"),
    parFieldPtrn = c("%s", "%s_A", "%s"), 
    stringsAsFactors = FALSE
  )
  
  kwb.utils::printIf(dbg, info)
  
  if (kind == "h") {
    clean_stop("data sources for hydraulic data not yet implemented.\n")
  } 
  
  if (kind == "r") {
    clean_stop("data sources for rain data not yet implemented.\n")
  }
  
  # Here the kind is expected to be "q" (quality)
  stopifnot(kind == "q")
  
  if (owner == "KWB") {
    
    ## Select row of meta data
    selected <- info$kind == kind & 
      info$owner == owner & 
      info$qua.level == qua.level
    
    infoRow <- info[selected, ]
    
    tbl <- sprintf(infoRow$tblPtrn, moniPoint, parName)
    tsField <- infoRow$tsField
    parField <- sprintf(infoRow$parFieldPtrn, parName)
    
    kwb.utils::printIf(dbg, infoRow)
    kwb.utils::catIf(dbg, sprintf(
      "tbl: %s\ntsField: %s\nparField: %s\n", tbl, tsField, parField
    ))
    
  } else if (owner == "SEN") {
    
  }
  
  # Return source info
  mdb <- miamdb(kind, moniPoint, qua.level, owner)
  
  list(mdb = mdb, tbl = tbl, tsField = tsField, parField = parField)
}
