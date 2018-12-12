# hsPlotAllDataAvailabilities --------------------------------------------------
#@2012-04-17;HSB;example removed
#hsPlotAllDataAvailabilities <- structure(function
hsPlotAllDataAvailabilities <- function # plot all MIA CSO data availabilities
### Plots availability of raw and validated data for different monitoring 
### points and parameters. For each monitoring point a pdf file 
### "hsDataAvailability_<MP>" where <MP> is the acronym of the monitoring point
### is created in the directory \eqn{strPdfDir}.
(
  moniPoints,
  ### vector containing names of monitoring points, e.g. c("TEG", "MUE")
  parNames,
  ### vector containing names of parameters, e.g. c("AFS", "CSB", "CSBf")
  dates, 
  ### vector containing a list of Date objects
  pdfDir
  ### path to output directory to which created pdf-files shall be written.
) 
{
  # Loop through monitoring points
  for (moniPoint in moniPoints) {
    
    pdf <- file.path(pdfDir, sprintf("hsDataAvailability_%s.pdf", moniPoint))
    cat("Writing plots to ", pdf, "...\n")
    
    # Open PDF graphics device
    #@2011-12-19: use hsPrepPdf
    hsPrepPdf(pdf, boolLandscape = FALSE, bordW = 2*2.41, bordH = 2*2.7)
    
    # mfrow: Prepare a grid of n x 1 plots with n being the number of parameters
    # mar: original margins plus 2 more lines at the bottom and 1 more on top
    # xpd: to allow legend to be outside of the plot region
    par(mfrow=c(length(moniPoints), 1), mar=par()$mar + c(2,0,1,0), xpd=TRUE)
    
    # Loop through different time periods
    for (i in 1:(length(dates) - 1)) {
      for (parName in parNames) {      
        hsPlotMiaCsoAvailabilities(
          c("r", "v"), moniPoint, parName, dates[i], dates[i+1])
      }
    }
    
    # Close PDF file
    dev.off()    
  }
}#,
# ex = function() {
#   ## Generate pdf files containing data availability plots for parameters
#   ## "AFS", "CSB", "CSBf", measured at monitoring points "MUE" (Muehlendamm), 
#   ## "TEG" (Tegeler Weg), "STA" (Stallstr.) within two different 
#   ## time intervals: 2010-01-01 to 2010-07-01 and  2011-01-01 to 2011-07-01.
#   hsPlotAllDataAvailabilities(c("MUE", "TEG", "STA"), c("AFS", "CSB", "CSBf"),
#     c(as.Date("2010-01-01"), as.Date("2010-07-01"), as.Date("2011-01-01"),
#       as.Date("2011-07-01")), tempdir())
#   
#   ## Show data availability plots for Muehlendamm in pdf viewer
#   pdfFile <- file.path(tempdir(), "hsDataAvailability_MUE.pdf")
#   system(paste(options("pdfviewer"), pdfFile))
# })

# hsPlotMiaCsoAvailabilities ---------------------------------------------------
hsPlotMiaCsoAvailabilities <- function
### Plots availability of raw and validated data as bar plot into one plot.
(
  qTypes, 
  ### vector of data quality type codes, e.g. c("r", "v", "c"): raw, validated 
  ### and calibrated data
  moniPoint, 
  ### name of monitoring point, e.g. "TEG", "MUE", "STA"
  parName, 
  ### name of parameter, e.g. "AFS", "CSB", "CSBf"
  dateFirst, 
  ### Date object representing first date to be considered
  dateLast
  ### Date object representing last date to be considered
) 
{
  cat("Plotting data availability of", parName, 
      "at monitoring point", moniPoint, 
      "within [", format.Date(dateFirst), ",", format.Date(dateLast), "]", "\n")
  
  legTxt <- NULL
  legCol <- NULL
  
  for (qType in qTypes) {
    
    # Get availability of raw/validated data
    avail <- hsMiaCsoDataAvailability(qType, moniPoint, parName, 
                                      dateFirst, dateLast) 
    
    # Set plot title
    main <- paste("Daily data availability of parameter", parName, 
                  "at monitoring point", moniPoint)
    
    # Set bar color according to type
    barCol <- switch(qType, r = "lightgreen", v = "darkgreen", c = "lightyellow")
    
    # Plot the availabilities
    hsPlotDataAvailability(avail, dateFirst, dateLast, colNames = parName,
                           main = main, labelStep = 7, col = barCol, 
                           add = (qType != "r"), cex.names = 1.0, border = NA)
    
    # Append type name/colour to list of legend texts/colours
    legTxt <- c(legTxt, ifelse(qType == "r", "raw data", 
                               ifelse(qType == "v", "validated data", 
                                      "calibrated data")))
    legCol <- c(legCol, barCol)
  }  
  
  # Add a legend to the plot...
  # inset=-0.1: 10% of plot height above the plot
  #@2011-12-19: adapt the legend to the type of the data status
  legend("top", legend = legTxt, fill = legCol, border = "white", box.col = NA, 
         inset = -0.15, horiz = TRUE)
  
  # Add horizontal lines at 0% and 100%
  # xpd=FALSE: only show inside plot area
  abline(h = c(0,100), xpd = FALSE, col = "lightgrey")
}

# hsMiaCsoDataAvailability -----------------------------------------------------
hsMiaCsoDataAvailability <- structure(
  function
  ### Data availability of raw/validated/calibrated data for parameter \emph{para},
  ### measured at monitoring point \emph{moniPoint} between \emph{dateFirst} 
  ### and \emph{dateLast}.
  (
    level, 
    ### one of "r" (= raw), "v" (= validated), "c" (= calibrated)
    moniPoint, 
    ### one of "STA" (= Stallstr.), "TEG" (= Tegeler Weg), "MUE" (= Muehlendamm)
    parName, 
    ### e.g. "AFS", "CSB", "CSBf", ...
    dateFirst = NULL, 
    ### Date object representing first date to be considered
    dateLast = NULL, 
    ### Date object representing last date to be considered
    tstep = NULL,
    ### expected time step between time stamps in seconds. Default: minimum 
    ### time difference found between consecutive timestamps in given interval  
    dbg = FALSE
    ### If TRUE, debug messages will be shown
  )
{ 
    # Get information on where to find the data (path to db, table name, ...)
    info <- hsDataSource(qua.level = level, moniPoint = moniPoint, 
                         parName = parName)
    
    #@2012-04-13;HSB;use hsDataAvailability instead of .old
    if (! is.null(dateFirst)) dateFirst = as.character(dateFirst)
    if (! is.null(dateLast))  dateLast  = as.character(dateLast)
    data <- hsMdbTimeSeries(info$mdb, info$tbl, info$tsField, info$parField,
                            minDate = dateFirst, maxDate = dateLast)
    hsDataAvailability(data, tstep, includeCount = FALSE, dbg = dbg)
    
    ## Redirect to hsDataAvailability.old
    #hsDataAvailability.old(info, dateFirst, dateLast, dbg = dbg)  
  },
ex = function() {
  ## Get data availability of raw data of parameter "CSBf", measured at
  ## monitoring point "TEG" (Tegeler Weg) between 2011-04-01 and 2011-10-01
  da <- hsMiaCsoDataAvailability("r", "TEG", "CSBf", "2011-04-01", "2011-10-01")
  head(da)
  
  ## Output:
  #   myInterval myCount   myAvail
  # 1 2011-04-01    1440 100.00000
  # 2 2011-04-02    1440 100.00000
  # 3 2011-04-03    1440 100.00000
  # 4 2011-04-04    1440 100.00000
  # 5 2011-04-05    1440 100.00000
  # 6 2011-04-06    1409  97.84722
})
