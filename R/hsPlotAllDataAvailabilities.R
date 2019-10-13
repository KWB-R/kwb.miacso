# hsPlotAllDataAvailabilities --------------------------------------------------

#' Plot all MIA CSO Data Availabilities
#' 
#' Plots availability of raw and validated data for different monitoring points
#' and parameters. For each monitoring point a pdf file
#' "hsDataAvailability_<MP>" where <MP> is the acronym of the monitoring point
#' is created in the directory \eqn{strPdfDir}.
#' 
#' 2012-04-17;HSB;example removed
#' 
#' @param moniPoints vector containing names of monitoring points, e.g. c("TEG",
#'   "MUE")
#' @param parNames vector containing names of parameters, e.g. c("AFS", "CSB",
#'   "CSBf")
#' @param dates vector containing a list of Date objects
#' @param pdfDir path to output directory to which created pdf-files shall be
#'   written.
#' @export
#' @examples 
#' \dontrun{
#' # Generate pdf files containing data availability plots for parameters 
#' # "AFS", "CSB", "CSBf", measured at monitoring points "MUE" (Muehlendamm),
#' # "TEG" (Tegeler Weg), "STA" (Stallstr.) within two different
#' # time intervals: 2010-01-01 to 2010-07-01 and  2011-01-01 to 2011-07-01.
#' hsPlotAllDataAvailabilities(
#'   c("MUE", "TEG", "STA"), 
#'   c("AFS", "CSB", "CSBf"),
#'   as.Date(c("2010-01-01", "2010-07-01", "2011-01-01", "2011-07-01")), 
#'   tempdir()
#' )
#' 
#' # Show data availability plots for Muehlendamm in pdf viewer
#' pdfFile <- file.path(tempdir(), "hsDataAvailability_MUE.pdf")
#' system(paste(options("pdfviewer"), pdfFile))
#' }
hsPlotAllDataAvailabilities <- function(moniPoints, parNames, dates, pdfDir) 
{
  # Loop through monitoring points
  for (moniPoint in moniPoints) {
    
    pdf <- file.path(pdfDir, sprintf("hsDataAvailability_%s.pdf", moniPoint))
    
    cat("Writing plots to ", pdf, "...\n")
    
    # Open PDF graphics device
    kwb.utils::preparePdf(
      pdf, landscape = FALSE, borderWidth.cm = 2*2.41, borderHeight.cm = 2*2.7
    )
    
    # mfrow: Prepare a grid of n x 1 plots with n being the number of parameters
    # mar: original margins plus 2 more lines at the bottom and 1 more on top
    # xpd: to allow legend to be outside of the plot region
    graphics::par(
      mfrow = c(length(moniPoints), 1), 
      mar = graphics::par()$mar + c(2, 0, 1, 0), 
      xpd = TRUE
    )
    
    # Loop through different time periods
    for (i in seq_len(length(dates) - 1)) {
      
      for (parName in parNames) {
        
        hsPlotMiaCsoAvailabilities(
          c("r", "v"), moniPoint, parName, dates[i], dates[i + 1]
        )
      }
    }
    
    # Close PDF file
    grDevices::dev.off()
  }
}

# hsPlotMiaCsoAvailabilities ---------------------------------------------------

#' Plots availability of raw and validated data as bar plot into one plot
#' 
#' @param qTypes vector of data quality type codes, e.g. c("r", "v", "c"): raw,
#'   validated and calibrated data
#' @param moniPoint name of monitoring point, e.g. "TEG", "MUE", "STA"
#' @param parName name of parameter, e.g. "AFS", "CSB", "CSBf"
#' @param dateFirst Date object representing first date to be considered
#' @param dateLast Date object representing last date to be considered
#' @export
hsPlotMiaCsoAvailabilities <- function(
  qTypes, moniPoint, parName, dateFirst, dateLast
) 
{
  cat(
    sprintf(
      "Plotting data availability of %s at monitoring point %s", 
      parName, moniPoint
    ),
    sprintf(
      "within [ %s , %s ]\n", 
      format.Date(dateFirst), format.Date(dateLast)
    )
  )
  
  legTxt <- NULL
  legCol <- NULL
  
  for (qType in qTypes) {
    
    # Get availability of raw/validated data
    avail <- hsMiaCsoDataAvailability(
      qType, moniPoint, parName, dateFirst, dateLast
    ) 
    
    # Set plot title
    main <- sprintf(
      "Daily data availability of parameter %s at monitoring point", 
      parName, moniPoint
    )
    
    # Set bar color according to type
    barCol <- switch(
      qType, r = "lightgreen", v = "darkgreen", c = "lightyellow"
    )
    
    # Plot the availabilities
    kwb.misc::hsPlotDataAvailability(
      avail, dateFirst, dateLast, colNames = parName, main = main, 
      labelStep = 7, col = barCol, add = (qType != "r"), cex.names = 1.0, 
      border = NA
    )
    
    # Append type name/colour to list of legend texts/colours
    legTxt <- c(
      legTxt, 
      c(r = "raw data", v = "validated data", c = "calibrated data")[qType]
    )
    
    legCol <- c(legCol, barCol)
  }  
  
  # Add a legend to the plot...
  # inset = -0.15: 15% of plot height above the plot
  graphics::legend(
    "top", legend = legTxt, fill = legCol, border = "white", box.col = NA, 
    inset = -0.15, horiz = TRUE
  )
  
  # Add horizontal lines at 0% and 100%
  # xpd = FALSE: only show inside plot area
  graphics::abline(h = c(0, 100), xpd = FALSE, col = "lightgrey")
}

