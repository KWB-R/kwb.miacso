# hsMoniPoints -----------------------------------------------------------------

#' Names of Available Monitoring Points
#' 
#' Returns the names of available monitoring points
#' 
#' @param kind kind of data: "q" = water quality, "h" = hydraulic data, "r" =
#'   rain
#' @param owner owner of the data, one of "KWB", "SEN", "BWB"
#' @export
hsMoniPoints <- function(kind = NULL, owner = "KWB") 
{  
  .check.kind(kind)   # stop if kind value is invalid
  .check.owner(owner) # stop if owner value is invalid
  
  if (kind == "h") {
    clean_stop("monitoring points of hydraulic data not yet implemented.\n")
  }
  
  if (kind == "r") {
    clean_stop("monitoring points of rain data not yet implemented.\n")
  }
  
  # Here we are dealing with water quality monitoring points
  stopifnot(kind == "q")
  
  if (owner == "KWB") {

    matrix(ncol = 2, byrow = TRUE, c(
      "STA", "Stallstr",
      "MUE", "Muehlendamm",
      "TEG", "Tegeler Weg"
    ))
    
  } else if (owner == "SEN") {
    
    # Names of senate monitoring points are found in tbl_Messstellen
    kwb.db::hsGetTable(
      mdb = qmdb("r", "STA", owner = "SEN"), 
      tbl = "tbl_Messstellen", 
      fields = "Abkuerzung, Langname", 
      dbg = FALSE
    )
    
  } else {
    
    matrix() # stop("No data of owner BWB available.\n")
  }
}

# miadir -----------------------------------------------------------------------

#' Experimental!
#' 
#' This function does not work! The intension was to get the full path to an MS
#' Access database file by filtering for certain criteria (owner, kind of data,
#' quality level (raw, valid, calibrated) and time resolution)...
#' 
#' @param owner data owner
#' @param kind kind of data (hydraulic, quality, rain)
#' @param quaLevel quality level (raw, validated, calibrated)
#' @param resol resolution
#' @param DS directory structure
#' @param depth depth
#' @param dbg whether to show debug messages or not
#' @export
miadir <- function(
  owner = NA, kind = NA, quaLevel = NA, resol = NA, 
  DS = kwb.misc::hsDirStructure(dbg = dbg), depth = 1, dbg = FALSE
)
{
  #  hsdbg()  
  
  msg <- sprintf(
    "owner = '%s', kind = '%s', qua.level = '%s', resol = '%s', depth = %d",
    owner, kind, quaLevel, resol, depth
  )
  
  kwb.utils::catIf(dbg, "in miadir(", msg, ")\n")

  if (depth > 10) {
    clean_stop("Maximum recursion depth (10) reached. Check tblDirStruct for endless ",
         "loops during recursive path resolution.\n")    
  }
  
  # number of criteria
  NCRIT <- as.integer((ncol(DS) - 1) / 2)
  
  # Find record(s) that correspond(s) to current set of argument values
  rec <- DS
  
  for (argname in c("owner", "kind", "quaLevel", "resol")) {
    
    argval = get(argname)
    
    if (! is.na(argval)) {
      
      rec <- rec[! is.na(rec[[argname]]) & rec[[argname]] == argval, ]    
      
      cat(sprintf("After filtering for %s == %s:\n", argname, argval))
      print(rec)            
    }
  }
  
  kwb.utils::catIf(dbg, "NCRIT:", NCRIT, "\n")
  kwb.utils::printIf(dbg, DS, "full directory structure")
  kwb.utils::printIf(dbg, rec, "filtered directory structure")
  
  if (nrow(rec) == 0) {
    clean_stop("No directory specified for these criteria: ", msg, "\n",
         "You need to specify at least one more criterion.\n")    
  }
  
  if (nrow(rec) > 1) {
    print(rec)
    clean_stop("Ambiguous combination of criteria (", msg, ")\n")
  }
  
  subdir  <- rec[1, NCRIT + 1]
  context <- rec[1, (NCRIT + 2):(2 * NCRIT + 1)]
  
  kwb.utils::printIf(dbg, context)
  kwb.utils::catIf(dbg, sprintf("subdir = '%s'\n", subdir))
  
  # Return the subdir if there is no context to resolve
  if (sum(is.na(context)) == NCRIT) {
    return(subdir)
  }
  
  # Otherwise resolve the context by calling miadir recursively and
  # append the subdir
  # It is important to call as.character separately on each element
  # of context. Otherwise, e.g. with as.character(context) NAs would
  # be converted to "NA"!
  resdir <- miadir(
    owner = as.character(context[1]),   
    kind = as.character(context[2]), 
    quaLevel = as.character(context[3]), 
    resol = as.character(context[4]), 
    depth = depth + 1, 
    dbg = dbg
  )
  
  file.path(resdir, subdir)
}

# miamdb2 ----------------------------------------------------------------------

#' Full Path to MIA CSO Database File
#' 
#' @param id optional. Integer number identifying the database file. If not 
#'   given the user is asked to enter a number on the console.
#' @return full path to database file or empty string \code{""} if an invalid
#'   \code{id} was given.
#' @export
miamdb2 <- function(id = 0)
{  
  # Read available MIA-CSO databases
  mdbs <- hsReadMiaMdbs()
  
  # If no ID is given let the user chose an id from the list of available ids
  if (id == 0) {  
    print(mdbs)
    cat("*** Select a database from the list above by its number or '0' to quit: ")
    id <- scan(n = 1, what = integer())
    cat(sprintf("Your choice: '%d'\n", id))
  }
  
  # Return an empty string if the id is invalid
  if (! id %in% seq_len(nrow(mdbs))) {
    cat("Invalid database id. Run miamdb2() without arguments to get",
        "a list of available ids.\n")
    return("")
  }
  
  # If the id is valid return the full path of the table in row <id>
  file.path(mdbs$mdbDir[id], mdbs$mdbFile[id])
}

# hsReadMiaMdbs ----------------------------------------------------------------

#' Read MIA-CSO databases
#' 
#' @param root root directory to start searching for new databases
#' @param search.new if TRUE, root directory is searched recursively for new
#'   databases; if FALSE databases are read from R meta database
#' @param dbg whether to show debug messages or not
#' @return data frame with columns \code{mdbFile}, \code{mdbDesc}, \code{mdbDir}
#' @export
hsReadMiaMdbs <- function(
  root, # = "//moby/miacso$/Daten/ACCESS", 
  search.new = FALSE, dbg = FALSE
) 
{
  mdbFields <- "mdbFile, mdbDesc, mdbDir, mdbAttrib"
  
  meta_db <- kwb.misc::mmdb()
  
  # Read mdb info from R meta db
  dfMdbs <- kwb.db::hsGetTable(
    meta_db, "tblMdbs", fields = mdbFields, dbg = dbg
  )
  
  # Update mdb path info if it is requested to search for new databases
  if (isTRUE(search.new)) {
    
    nNew <- hsUpdateMiaMdbs(dfMdbs, root)
    
    # if database paths have been added reread mdb info from meta db
    if (nNew > 0) {
      cat(nNew, "database paths have been added.\n")
      dfMdbs <- kwb.db::hsGetTable(meta_db, "tblMdbs", fields = mdbFields)
    }
  }
  
  dfMdbs
}

# hsUpdateMiaMdbs --------------------------------------------------------------

#' Update Metadata Database
#' 
#' @param dfMdbs data frame with columns \code{mdbDir}, \code{mdbFile} 
#'   containing paths to currently known databases
#' @param root path to directory from which to start looking recursively for 
#'   MS Access database files
#' @return Return number n of added database paths
#' @export
hsUpdateMiaMdbs <- function(dfMdbs, root) 
{  
  # Create full paths to currently known dbs
  mdbsCur <- file.path(dfMdbs$mdbDir, dfMdbs$mdbFile)
  
  # Search recursively for new databases
  mdbsNew <- dir(root, pattern = "\\.mdb$", recursive = TRUE, full.names = TRUE)
  
  # Wee need to split and reassemble the path in order to let the dirname 
  # function convert "//" at the beginning of a path into "\\" as the path is
  # stored in the database (because it was got with the dirname function, too).
  mdbsNew <- file.path(dirname(mdbsNew), basename(mdbsNew))
  
  # Which of the databases found are not yet documented in the meta db?
  mdbsNew <- mdbsNew[! mdbsNew %in% mdbsCur]
  
  # Number of new databases
  n <- length(mdbsNew)
  
  # Return 0 to indicate that there are no new databases
  if (n == 0) {
    return(0)
  }
  
  # If there are new databases ask the user if their paths shall be appended
  # to tblMdbs in R meta db
  msg <- paste(mdbsNew, collapse = "\n")
  
  cat(
    sprintf("\n%s\nShall I append the above %d database paths", msg, n), 
    "to the list of known databases (Y,n)? "
  )
  
  answer <- scan(n = 1, what = character())
  
  cat(sprintf("Your choice: '%s'\n", answer))
  
  # Return 0 (no new databases appended) if the user did not answer "Yes"
  if (answer != "Y") {
    return(0)
  }
  
  # Create data frame containing paths to newly found databases
  dfMdbs <- data.frame(
    mdbFile = basename(mdbsNew), 
    mdbDir  = dirname(mdbsNew),
    mdbDesc = sprintf("imported by R on %s", Sys.time())
  )
  
  # Create a new table tblTmpMdbsNew in R meta db... 
  kwb.db::hsPutTable(kwb.misc::mmdb(), dfMdbs, "tblTmpMdbsNew")
  
  # ... append its records to tblMdbs
  sql <- paste(
    "INSERT INTO tblMdbs(mdbDir, mdbFile, mdbDesc) ",
    "SELECT mdbDir, mdbFile, mdbDesc FROM tblTmpMdbsNew"
  )
  
  kwb.db::hsSqlQuery(kwb.misc::mmdb(), sql)
  
  # ... and delete the temporarily created table tblTmpMdbsNew
  kwb.db::hsDropTable(kwb.misc::mmdb(), "tblTmpMdbsNew")
  
  # Return number n of added database paths
  n
}

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
  basePath <- "//moby/miacso$/Daten/ACCESS/KwbMonitoring"
  tbl <- tsField <- parField <- ""
  
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
  .check.kind(kind) # stop if kind value is invalid
  .check.qua.level(qua.level) # stop if qua.level value is invalid
  .check.moniPoint(moniPoint, kind = kind, owner = owner) # stop if monitoring point is invalid
  
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
  
  if (kind == "q") { 
    
    if (owner == "KWB") {
      
      ## Select row of meta data
      infoRow <- info[
        info$kind == kind & info$owner == owner & info$qua.level == qua.level, ]
      
      tbl      <- sprintf(infoRow$tblPtrn, moniPoint, parName)
      tsField  <- infoRow$tsField
      parField <- sprintf(infoRow$parFieldPtrn, parName)
      
      kwb.utils::printIf(dbg, infoRow)
      kwb.utils::catIf(dbg, sprintf(
        "tbl: %s\ntsField: %s\nparField: %s\n", tbl, tsField, parField
      ))
      
    } else if (owner == "SEN") {
      
    }
    
  } else if (kind == "h") {
    
    clean_stop("data sources for hydraulic data not yet implemented.\n")
    
  } else if (kind == "r") {
    
    clean_stop("data sources for rain data not yet implemented.\n")
  }
  
  # Return source info
  mdb <- miamdb(kind, moniPoint, qua.level, owner) 
  list(mdb = mdb, tbl = tbl, tsField = tsField, parField = parField)
}

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
  .check.kind(kind) # stop if kind value is invalid
  .check.moniPoint(moniPoint, kind = kind, owner = owner) # stop if monitoring point is invalid
  .check.qua.level(qua.level) # stop if qua.level value is invalid
  
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
        ptrn <- sprintf("^KWB_%s_ScanPar_(.*)_VAL$", moniPoint) # Search pattern
        if (dbg) cat("pattern: ", ptrn, "\n")
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
      "\nUsage: hsGetFpAndValRaw(moniPoint, parNames, year, firstDate, lastDate)",
      "  moniPoint: acronym of monitoring point: \"STA\" (Stallstr.), \"TEG\" (Tegeler Weg) or \"MUE\" (Muehlendamm)",
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
    file.path("D:", "_Hauke", "tmp", mdb),
    file.path(
      "C:", "Users", "hsonne", "_EigeneDateien", "_Projekte_lokal", 
      "_Projekt_MIA_CSO_lokal", "DbDevelopment", "DbData", "RAW_Scan", mdb
    )
  )
  
  # Create string containing the list of fields to be selected
  fields <- gsub(".", "_", hsFpFields(moniPoint), fixed = TRUE)
  pars <- paste(parNames, collapse = ",")
  
  # Generate SQL string
  sql <- sprintf(
    "SELECT myDateTime, %s, %s FROM %s WHERE %s AND %s",
    pars, fields, tbl,
    sprintf("NOT (%s)", kwb.db::hsSqlExOr(parNames, "IsNull")), # validity condition
    kwb.db::hsSqlExTimeCond(
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
    dts = res[,1], pars = as.data.frame(res[pars]),
    fps = as.matrix(res[,(i+1):ncol(res)])
  )
}

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
      "  moniPoint: acronym of monitoring point: \"STA\" (Stallstr.), \"TEG\" (Tegeler Weg) or \"MUE\" (Muehlendamm)",
      "  parName:     parameter name, e.g. \"AFS\"",
      "  firstDate: first date of requested time period (mm/dd/yyyy)",
      "  lastDate:  last date of requested time period (mm/dd/yyyy)",
      "Returns a data.frame containing the validated data, directly drawn from the current databases.",
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
  #myData <- sqlQuery(channel, sql)
  #myData
  kwb.db::hsSqlQuery(src$mdb, sql)
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
