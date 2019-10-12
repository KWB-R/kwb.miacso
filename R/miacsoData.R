# Shortcut functions -----------------------------------------------------------
hmdb <- function # hydraulic mdbs
### hydraulic mdbs
(...) miamdb('h', ...) 

qmdb <- function # (water) quality mdbs
### (water) quality mdbs
(...) miamdb('q', ...) 

rmdb <- function # rain mdbs
### rain mdbs
(...) miamdb('r', ...) 

# miamdb -----------------------------------------------------------------------
miamdb <- function
### Returns full path to a MIA-CSO project database.
(
  kind = NULL,
  ### kind of data: "q" = water quality, "h" = hydraulic data, "r" = rain
  moniPoint = NULL, 
  ### name of monitoring point, e.g. "STA", "TEG", "MUE"
  qua.level = NULL,
  ### data quality level ("r" = raw, "v" = validated, "c" = calibrated) 
  owner = "KWB"
  ### owner of the data, one of "KWB", "SEN", "BWB"  
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
    }
    else if (owner == "SEN") {
      cat("WARNING: no distinction between raw, valid and calibrated data for owner 'SEN'.\n")
      file.path("//moby/miacso$/Daten/ACCESS/Gewaesserguete/kontinuierlich",
                "Messreihen", "Hauptparameter_Senat.mdb")
    }
    else {
      stop("No water quality data of owner BWB available.\n")      
    }
  }
  else if (kind == "h") { # hydraulic data
    stop("mdb paths for hydraulic data not yet implemented.\n")
  }
  else if (kind == "r") {
    stop("mdb paths for rain data not yet implemented.\n")
  }
  ### Returns full path to Access database containing the specified kind of data
}

# .check.kind ------------------------------------------------------------------
.check.kind <- function(kind = NULL) 
{  
  msg <- "(Data) kind must be one of \'q\' (water quality), \'h\' (hydraulic) or \'r\' (rain).\n"
  
  if (is.null(kind)) 
    stop("No (data) kind given. ", msg)
  
  if (! kind %in% c("q", "h", "r")) 
    stop(msg)
}

# .check.owner -----------------------------------------------------------------
.check.owner <- function(owner = NULL) 
{  
  msg <- "(Data) owner must be one of \'KWB\', \'SEN\' (Senate) or \'BWB\'.\n"
  
  if (is.null(owner)) 
    stop("No (data) owner given. ", msg)
  
  if (! owner %in% c("KWB", "SEN", "BWB")) 
    stop(msg)
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

# hsMoniPoints -----------------------------------------------------------------
hsMoniPoints <- function
### Returns names of available monitoring points
(
  kind = NULL,
  ### kind of data: "q" = water quality, "h" = hydraulic data, "r" = rain
  owner = "KWB"
  ### owner of the data, one of "KWB", "SEN", "BWB"
) 
{  
  .check.kind(kind)   # stop if kind value is invalid
  .check.owner(owner) # stop if owner value is invalid
  
  if (kind == "q") {
    if (owner == "KWB") {
      mps <- matrix(c("STA", "Stallstr",
                      "MUE", "Muehlendamm",
                      "TEG", "Tegeler Weg"), ncol = 2, byrow = TRUE)
    } else if (owner == "SEN") {
      # Names of senate monitoring points are found in tbl_Messstellen
      mps <- kwb.db::hsGetTable(
        qmdb("r", "STA", owner = "SEN"), "tbl_Messstellen", 
        fields = "Abkuerzung, Langname", dbg = FALSE
      )
    } else {
      mps <- matrix() # stop("No data of owner BWB available.\n")
    }
  }
  else if (kind == "h") {
    stop("monitoring points of hydraulic data not yet implemented.\n")
  }
  else if (kind == "r") {
    stop("monitoring points of rain data not yet implemented.\n")
  }
  
  mps
}

# miadir -----------------------------------------------------------------------
miadir <- function # miadir
### miadir. Does not work! The intension was to get the full path to an MS
### Access database file by filtering for certain criteria (owner, kind of data,
### quality level (raw, valid, calibrated) and time resolution)...
(
  owner = NA,
  kind = NA, 
  quaLevel = NA, 
  resol = NA, 
  DS = hsDirStructure(dbg = dbg), 
  depth = 1, 
  dbg = FALSE
)
{
  #  hsdbg()  
  
  msg <- sprintf("owner = '%s', kind = '%s', qua.level = '%s', resol = '%s', depth = %d",
                 owner, kind, quaLevel, resol, depth)
  
  if (dbg) {
    cat("in miadir(", msg, ")\n")
  }
  
  if (depth > 10) {
    stop("Maximum recursion depth (10) reached. Check tblDirStruct for endless ",
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
  
  if (dbg) {
    cat("NCRIT:", NCRIT, "\n")
    cat("full directory structure:\n")
    print(DS)
    cat("filtered directory structure:\n")
    print(rec)
  }
  
  if (nrow(rec) == 0) {
    stop("No directory specified for these criteria: ", msg, "\n",
         "You need to specify at least one more criterion.\n")    
  }

  if (nrow(rec) > 1) {
    print(rec)
    stop("Ambiguous combination of criteria (", msg, ")\n")
  }
  
  subdir  <- rec[1, NCRIT + 1]
  context <- rec[1, (NCRIT + 2):(2 * NCRIT + 1)]
  
  if (dbg) {
    cat("context:\n")
    print(context)
    cat(sprintf("subdir = '%s'\n", subdir))
  }
  
  # Return the subdir if there is no context to resolve
  if (sum(is.na(context)) == NCRIT) {
    return(subdir)
  }
  
  # Otherwise resolve the context by calling miadir recursively and
  # append the subdir
  # It is important to call as.character separately on each element
  # of context. Otherwise, e.g. with as.character(context) NAs would
  # be converted to "NA"!
  resdir <- miadir(owner = as.character(context[1]),   
                   kind = as.character(context[2]), 
                   quaLevel = as.character(context[3]), 
                   resol = as.character(context[4]), 
                   depth = depth + 1, 
                   dbg = dbg)
  
  file.path(resdir, subdir)
}

# miamdb2 ----------------------------------------------------------------------
miamdb2 <- function # miamdb2
### miamdb2
(
  id = 0
)
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
  if (! id %in% 1:nrow(mdbs)) {
    cat ("Invalid database id. Run miamdb2() without arguments to get",
         "a list of available ids.\n")
    return("")
  }
  
  # If the id is valid return the full path of the table in row <id>
  file.path(mdbs$mdbDir[id], mdbs$mdbFile[id])
}

# hsReadMiaMdbs ----------------------------------------------------------------
hsReadMiaMdbs <- function # read MIA-CSO databases
### read MIA-CSO databases
(
  root, # = "//moby/miacso$/Daten/ACCESS", 
  ### root directory to start searching for new databases
  search.new = FALSE,
  ### if TRUE, root directory is searched recursively for new databases;
  ### if FALSE databases are read from R meta database
  dbg = FALSE
) 
{
  mdbFields <- "mdbFile, mdbDesc, mdbDir, mdbAttrib"
  
  # Read mdb info from R meta db
  dfMdbs <- kwb.db::hsGetTable(mmdb(), "tblMdbs", fields = mdbFields, dbg = dbg)
  
  # Update mdb path info if it is requested to search for new databases
  if (isTRUE(search.new)) {
    
    nNew <- hsUpdateMiaMdbs(dfMdbs, root)
    
    # if database paths have been added reread mdb info from meta db
    if (nNew > 0) {
      cat(nNew, "database paths have been added.\n")
      dfMdbs <- kwb.db::hsGetTable(mmdb(), "tblMdbs", fields = mdbFields)    
    }
  }
  
  dfMdbs
  ### data frame with columns \emph{mdbFile}, \emph{mdbDesc}, \emph{mdbDir}
}

# hsUpdateMiaMdbs --------------------------------------------------------------
hsUpdateMiaMdbs <- function # hsUpdateMiaMdbs
### hsUpdateMiaMdbs
(
  dfMdbs, root
) 
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
  
  cat(sprintf("\n%s\nShall I append the above %d database paths", msg, n), 
      "to the list of known databases (Y,n)? ")
  
  answer <- scan(n = 1, what = character())
  
  cat(sprintf("Your choice: '%s'\n", answer))
  
  # Return 0 (no new databases appended) if the user did not answer "Yes"
  if (answer != "Y") {
    return(0)
  }
  
  # Create data frame containing paths to newly found databases
  dfMdbs <- data.frame(mdbFile = basename(mdbsNew), mdbDir  = dirname(mdbsNew),
                       mdbDesc = sprintf("imported by R on %s", Sys.time()))
  
  # Create a new table tblTmpMdbsNew in R meta db... 
  kwb.db::hsPutTable(mmdb(), dfMdbs, "tblTmpMdbsNew")
  
  # ... append its records to tblMdbs
  sql <- paste("INSERT INTO tblMdbs(mdbDir, mdbFile, mdbDesc) ",
               "SELECT mdbDir, mdbFile, mdbDesc FROM tblTmpMdbsNew")
  
  kwb.db::hsSqlQuery(mmdb(), sql)
  
  # ... and delete the temporarily created table tblTmpMdbsNew
  kwb.db::hsDropTable(mmdb(), "tblTmpMdbsNew")
  
  # Return number n of added database paths
  n
}

# hsDataSource -----------------------------------------------------------------
hsDataSource <- structure(function
                          ### Returns a list containing 1. full path to Access database, 2. table name, 
                          ### 3. name of timestamp field, 4. name of parameter field for the table that 
                          ### contains data of parameter \emph{parName}, measured at monitoring point 
                          ### \emph{moniPoint} in data quality level \emph{qua.level}.
                          (
                            qua.level = NULL,
                            ### data quality level ("r" = raw, "v" = validated, "c" = calibrated) 
                            moniPoint = NULL, 
                            ### name of monitoring point, e.g. "STA", "TEG", "MUE"
                            parName,
                            ### name of parameter, e.g. "AFS", "CSB", "CSBf"
                            kind = "q",
                            ### kind of data: "q" = water quality, "h" = hydraulic data, "r" = rain
                            owner = "KWB",  
                            ### owner of the data, one of "KWB", "SEN", "BWB"  
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
                            #    stop(msg)
                            #  }
                            
                            #@@2012-04-12;HSB
                            .check.owner(owner)
                            .check.kind(kind)                 # stop if kind value is invalid
                            .check.qua.level(qua.level)      # stop if qua.level value is invalid
                            .check.moniPoint(moniPoint, kind = kind, owner = owner) # stop if monitoring point is invalid
                            
                            ## provide meta data in data frame "info"
                            info <- data.frame(
                              kind         = c("q",   "q",   "q"),
                              owner        = c("KWB", "KWB", "KWB"),
                              qua.level    = c("r",   "v",   "c"),
                              tblPtrn      = c("qry_KWB_%s_ScanPar",    # raw
                                               "KWB_%s_ScanPar_%s_VAL", # validated
                                               "qry_%s_CAL_%s"),        # calibrated
                              tsField      = c("myDateTime", "myDateTime", "myDateTime"),
                              parFieldPtrn = c("%s", "%s_A", "%s"), stringsAsFactors = FALSE)
                            
                            if (dbg)
                              print(info)
                            
                            if (kind == "q") { 
                              if (owner == "KWB") {
                                
                                ## Select row of meta data
                                infoRow <- info[
                                  info$kind == kind & info$owner == owner & info$qua.level == qua.level, ]
                                
                                tbl      <- sprintf(infoRow$tblPtrn, moniPoint, parName)
                                tsField  <-         infoRow$tsField
                                parField <- sprintf(infoRow$parFieldPtrn, parName)
                                
                                if (dbg) {
                                  cat("infoRow:\n")
                                  print(infoRow)
                                  cat(sprintf("tbl: %s\ntsField: %s\nparField: %s\n", 
                                              tbl, tsField, parField))
                                }
                              }
                              else if (owner == "SEN") {
                              }
                            }
                            else if (kind == "h") {
                              stop("data sources for hydraulic data not yet implemented.\n")
                            }
                            else if (kind == "r") {
                              stop("data sources for rain data not yet implemented.\n")
                            }
                            
                            # Return source info
                            mdb <- miamdb(kind, moniPoint, qua.level, owner) 
                            list(mdb = mdb, tbl = tbl, tsField = tsField, parField = parField)
                            ### Returns a list with the following named elements:
                            ### 1. \emph{mdb}: full path to Access database, 2. \emph{tbl}: table name, 
                            ### 3. \emph{tsField}: name of timestamp field, 4. \emph{parField}: 
                            ### name of parameter field
                          },
ex = function() {
  ## Get source information of validated data of parameter AFS from Stallstr. 
  si <- hsDataSource("v", "STA", "AFS")
  si 
  ## ouput:
  #  $db
  #  [1] "//moby/miacso$/Daten/ACCESS/KwbMonitoring/2VAL/KWB_STA_VAL.mdb"
  # 
  #  $tbl
  #  [1] "KWB_STA_ScanPar_AFS_VAL"
  # 
  #  $tsfield
  #  [1] "myDateTime"
  # 
  #  $parfield
  #  [1] "AFS_A"
  
  ## Access the pieces of information with the $ operator:
  si$mdb # [1] "//moby/miacso$/Daten/ACCESS/KwbMonitoring/2VAL/KWB_STA_VAL.mdb"
  si$tbl # [1] "KWB_STA_ScanPar_AFS_VAL"
})

# hsPars -----------------------------------------------------------------------
hsPars <- function
### Returns names of available parameters
(
  kind = NULL,
  ### kind of data: "q" = water quality, "h" = hydraulic data, "r" = rain
  moniPoint = NULL,
  ### name of monitoring point, e.g. "STA", "TEG", "MUE"
  qua.level = "c",
  ### data quality level ("r" = raw, "v" = validated, "c" = calibrated)
  owner = "KWB",
  ### owner of the data, one of "KWB", "SEN", "BWB"
  dbg = TRUE
) {
  
  .check.owner(owner)
  .check.kind(kind)                 # stop if kind value is invalid
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
      }
      else if (qua.level == "v") {
        # Take available parameter names from table names in valid database
        mdb <- miamdb(kind, moniPoint, qua.level = "v")
        tables <- kwb.db::hsTables(mdb) # all table names
        
        # Search for tables "KWB_<moniPoint>_ScanPar_<parName>_VAL" and
        # cut the <parName>
        ptrn <- sprintf("^KWB_%s_ScanPar_(.*)_VAL$", moniPoint) # Search pattern
        if (dbg) cat("pattern: ", ptrn, "\n")
        gsub(ptrn, "\\1", grep(ptrn, tables, value = TRUE))
      }
      else {
        stop("available names of calibrated parameters not yet implemented.\n")
      }
    }
    else if (owner == "SEN") {
      
    }
    else {
    }    
  }
  else if (kind == "h") { # hydraulic parameters
  }
  else if (kind == "r") { # rain parameters
  }
}

# hsGetFpAndValRaw -------------------------------------------------------------
hsGetFpAndValRaw <- function
### For the given parameters, the values and fingerprints are read from the
### database.
(
  moniPoint,
  ### name of monitoring point
  parNames,
  ### vector of parameter names
  year,
  ### year of which data is requested
  firstDate,
  ### first date of requested time period
  lastDate,
  ### first date of requested time period
  home = FALSE
  ### if true, haukes home path is used instead of kwb office path
) 
{
  if (missing(moniPoint)) {
    msg <- paste("\nUsage: hsGetFpAndValRaw(moniPoint, parNames, year, firstDate, lastDate)",
                 "  moniPoint: acronym of monitoring point: \"STA\" (Stallstr.), \"TEG\" (Tegeler Weg) or \"MUE\" (Muehlendamm)",
                 "  parNames: vector of parameter names, e.g. c(\"AFS\", \"CSB\")",
                 "  year: year of which data is requested",
                 "  firstDate: first date of requested time period (mm/dd/yyyy)",
                 "  lastDate: last date of requested time period (mm/dd/yyyy)",
                 "Returns a data.frame with elements:",
                 "  \"dts\":  timestamps,",
                 "  \"pars\": parameter values and",
                 "  \"fps\":  fingerprints",
                 sep = "\n")
    stop(msg)
  }
  
  # Determine database name
  mdb <- sprintf("KWB_%s_Scan_Joined.mdb", moniPoint)
  
  # Determine table name
  tbl <- paste("tbl_KWB_", moniPoint, "_ScanFP_Par_", year, sep="")
  
  mdbFullPath <- ifelse(
    home,
    file.path("D:", "_Hauke", "tmp", mdb),
    file.path("C:", "Users", "hsonne", "_EigeneDateien",
              "_Projekte_lokal", "_Projekt_MIA_CSO_lokal", "DbDevelopment",
              "DbData", "RAW_Scan", mdb))
  
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
  list(dts = res[,1], pars = as.data.frame(res[pars]),
       fps = as.matrix(res[,(i+1):ncol(res)]))
  ### Return the query result as a new data.frame (which is a special type of a
  ### list) with elements "dts" (date-times), "pars" (parameter values) and
  ### "fps" (fingerprints)
}

# hsGetValData -----------------------------------------------------------------
hsGetValData <- function
### Get validated data from validated database.
# 2011-10-25: created
(
  moniPoint,
  ### name of monitoring point, e.g. "TEG", "STA", "MUE"
  parName,
  ### parameter name, e.g. "AFS"
  firstDate,
  ### first date to be selected as "mm/dd/yyyy hh:nn:ss"
  lastDate
  ### lastDate: last date to be selected as "mm/dd/yyyy hh:nn:ss"
) 
{
  if (missing(moniPoint)) {
    msg <- paste("\nUsage: hsGetValData(moniPoint, parName, firstDate, lastDate)",
                 "  moniPoint: acronym of monitoring point: \"STA\" (Stallstr.), \"TEG\" (Tegeler Weg) or \"MUE\" (Muehlendamm)",
                 "  parName:     parameter name, e.g. \"AFS\"",
                 "  firstDate: first date of requested time period (mm/dd/yyyy)",
                 "  lastDate:  last date of requested time period (mm/dd/yyyy)",
                 "Returns a data.frame containing the validated data, directly drawn from the current databases.",
                 sep = "\n")
    stop(msg)
  }
  
  # Provide path to database, table name and table field names
  src <- hsDataSource("v", moniPoint, parName)
  
  # Create string containing the list of fields to be selected
  fieldList <- sprintf("%s, %s_A, %s_C", src$tsField, parName, parName)
  
  # Validity condition
  condValidity <- sprintf("NOT (%s_A IS NULL AND %s_C IS NULL)", parName, parName)
  
  # No condition at all!
  condValidity <- "TRUE"
  
  # Generate SQL string
  sql <- sprintf("SELECT %s FROM %s WHERE %s AND %s BETWEEN #%s# AND #%s#",
                 fieldList, src$tbl, condValidity, src$tsField, firstDate, lastDate)
  print(sql)
  
  ## Run the SQL query
  #myData <- sqlQuery(channel, sql)
  #myData
  kwb.db::hsSqlQuery(src$mdb, sql)
  ### data.frame containing validated data
}

# hsFpFields -------------------------------------------------------------------
hsFpFields <- function
### String representing comma separated list of fields in fingerprint table
(
  moniPoint  ##<< acronym of monitoring point: "STA", "TEG" or "MUE"
)
{
  sprintf("e%s",
          paste(seq(200, hsLastWL(moniPoint), 2.5), collapse=",e"))
  ### String representing comma separated list of fields in fingerprint table
}

# hsLastWL ---------------------------------------------------------------------
hsLastWL <- function
### Returns the last available wavelength for the given monitoring point.
#@2011-12-19: moved from hsLibFingerprint.r
(
  moniPoint
  ### Name of monitoring point, e.g. "TEG", "STA", "MUE"
) 
{
  lastWL <- ifelse(moniPoint == "TEG", 750,
                   ifelse(moniPoint == "STA", 740,
                          ifelse(moniPoint == "MUE", 732.5, 0)))
  
  if (lastWL == 0) {
    print(paste("Unknown monitoring point:", moniPoint))
  }
  
  # Return lastWL
  lastWL
  ### Last wavelength being provided by spectrometer at given monitoring point
}

# hsWavelengthAtIndex ----------------------------------------------------------
hsWavelengthAtIndex <- function
### Returns the wavelength in nm that belongs to the given column index i.
#@2011-12-19: moved from hsLibFingerprint.r
(
  i
  ### Index (i=1: 200nm, i=2: 202.5nm, ..., i=217: 740nm).
) 
{
  200 + (i-1) * 2.5
  ### Wavelength corresponding to index
}

# hsIndexOfWavelength ----------------------------------------------------------
hsIndexOfWavelength <- function
### Returns the index at which the wavelength given in nm can be found in a
### vector (i=1: 200nm, i=2: 202.5nm, ..., i=217: 740nm).
#@2011-12-19: moved from hsLibFingerprint.r
(
  wavelength
  ### Wavelength for which corresponding index shall be determined
) 
{
  (wavelength - 200) / 2.5 + 1
  ### Index corresponding the given wavelength
}
