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
    
    cat(
      "*** Select a database from the list above by its number or '0' to quit: "
    )
    
    id <- scan(n = 1, what = integer())
    
    cat(sprintf("Your choice: '%d'\n", id))
  }
  
  # Return an empty string if the id is invalid
  if (! id %in% seq_len(nrow(mdbs))) {
    
    cat(
      "Invalid database id. Run miamdb2() without arguments to get a list of",
      "available ids.\n"
    )
    
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
