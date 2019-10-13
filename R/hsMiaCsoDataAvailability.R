# hsMiaCsoDataAvailability -----------------------------------------------------

#' Provide Information on Data Availabilities
#' 
#' Data availability of raw/validated/calibrated data for parameter \code{para},
#' measured at monitoring point \code{moniPoint} between \code{dateFirst} and
#' \code{dateLast}.
#' 
#' @param level one of "r" (= raw), "v" (= validated), "c" (= calibrated)
#' @param moniPoint one of "STA" (= Stallstr.), "TEG" (= Tegeler Weg), "MUE" (=
#'   Muehlendamm)
#' @param parName e.g. "AFS", "CSB", "CSBf", ...
#' @param dateFirst Date object representing first date to be considered
#' @param dateLast Date object representing last date to be considered
#' @param tstep expected time step between time stamps in seconds. Default:
#'   minimum time difference found between consecutive timestamps in given
#'   interval
#' @param dbg If \code{TRUE}, debug messages will be shown
#' @export
#' @examples 
#' \dontrun{
#' # Get data availability of raw data of parameter "CSBf", measured at
#' # monitoring point "TEG" (Tegeler Weg) between 2011-04-01 and 2011-10-01
#' da <- hsMiaCsoDataAvailability("r", "TEG", "CSBf", "2011-04-01", "2011-10-01")
#' head(da)
#' }
#' # Output:
#' #   myInterval myCount   myAvail
#' # 1 2011-04-01    1440 100.00000
#' # 2 2011-04-02    1440 100.00000
#' # 3 2011-04-03    1440 100.00000
#' # 4 2011-04-04    1440 100.00000
#' # 5 2011-04-05    1440 100.00000
#' # 6 2011-04-06    1409  97.84722
hsMiaCsoDataAvailability <- function(
  level, moniPoint, parName, dateFirst = NULL, dateLast = NULL, tstep = NULL, 
  dbg = FALSE
)
{ 
  # Get information on where to find the data (path to db, table name, ...)
  info <- hsDataSource(
    qua.level = level, 
    moniPoint = moniPoint, 
    parName = parName
  )
  
  #@2012-04-13;HSB;use hsDataAvailability instead of .old
  if (! is.null(dateFirst)) {
    dateFirst <- as.character(dateFirst)
  }
  
  if (! is.null(dateLast)) {
    dateLast <- as.character(dateLast)
  }
  
  data <- kwb.db::hsMdbTimeSeries(
    info$mdb, info$tbl, info$tsField, info$parField,
    minDate = dateFirst, maxDate = dateLast
  )
  
  kwb.misc::hsDataAvailability(data, tstep, includeCount = FALSE, dbg = dbg)
  #kwb.misc::hsDataAvailability.old(info, dateFirst, dateLast, dbg = dbg)  
}
