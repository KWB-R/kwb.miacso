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
  # Stop on invalid inputs
  .check.kind(kind)
  .check.owner(owner)
  
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
