# hsPars -----------------------------------------------------------------------

#' Names of available parameters
#' 
#' @param kind kind of data: "q" = water quality, "h" = hydraulic data, "r" =
#'   rain
#' @param moniPoint name of monitoring point, e.g. "STA", "TEG", "MUE"
#' @param qua.level data quality level ("r" = raw, "v" = validated, "c" =
#'   calibrated)
#' @param owner owner of the data, one of "KWB", "SEN", "BWB"
#' @param dbg whether to show debug messages or not
#' @return Returns the names of available parameters
#' @export
hsPars <- function(
  kind = NULL, moniPoint = NULL, qua.level = "c", owner = "KWB", dbg = TRUE
)
{
  .check.owner(owner)
  .check.kind(kind)
  .check.moniPoint(moniPoint, kind = kind, owner = owner)
  .check.qua.level(qua.level)
  
  if (kind == "q") { # water quality parameters
    
    if (owner == "KWB") {
      
      if (qua.level == "r") {
        
        ds <- hsDataSource(qua.level, kind = kind, moniPoint = moniPoint, dbg = dbg)
        
        if (dbg) print(ds)
        
        # Take available parameter names from table field names in
        # raw database table
        fields <- kwb.db::hsFields(ds$mdb, ds$tbl, dbg = dbg)
        
        # Exclude the first column (timestamp)
        fields[2:length(fields)]
        
      } else if (qua.level == "v") {
        
        # Take available parameter names from table names in valid database
        mdb <- miamdb(kind, moniPoint, qua.level = "v")
        tables <- kwb.db::hsTables(mdb) # all table names
        
        # Search for tables "KWB_<moniPoint>_ScanPar_<parName>_VAL" and
        # cut the <parName>
        
        # Search pattern
        ptrn <- sprintf("^KWB_%s_ScanPar_(.*)_VAL$", moniPoint)
        
        kwb.utils::catIf(dbg, "pattern: ", ptrn, "\n")
        
        gsub(ptrn, "\\1", grep(ptrn, tables, value = TRUE))
        
      } else {
        
        clean_stop(
          "available names of calibrated parameters not yet implemented.\n"
        )
      }
      
    } else if (owner == "SEN") {
    } else {
    }    
  } else if (kind == "h") { # hydraulic parameters
  } else if (kind == "r") { # rain parameters
  }
}
