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
  #kind=NA;quaLevel=NA;resol=NA;depth=1;dbg=TRUE
  msg <- sprintf(
    "owner = '%s', kind = '%s', qua.level = '%s', resol = '%s', depth = %d",
    owner, kind, quaLevel, resol, depth
  )
  
  kwb.utils::catIf(dbg, "in miadir(", msg, ")\n")
  
  if (depth > 10) {
    clean_stop(
      "Maximum recursion depth (10) reached. Check tblDirStruct for endless ",
      "loops during recursive path resolution.\n"
    )
  }
  
  # number of criteria
  NCRIT <- as.integer((ncol(DS) - 1) / 2)
  
  # Find record(s) that correspond(s) to current set of argument values
  rec <- DS
  
  for (argname in c("owner", "kind", "quaLevel", "resol")) {
  
    #argname <- "owner"  
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
    clean_stop(
      "No directory specified for these criteria: ", msg, "\n",
      "You need to specify at least one more criterion.\n"
    )
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
    dbg = dbg,
    DS = DS
  )
  
  file.path(resdir, subdir)
}
