# hsLastWL ---------------------------------------------------------------------

#' Last Available Wavelength for Given Monitoring Point
#' 
#' Returns the last available wavelength of the spectrometer installed at the 
#' given monitoring point.
#' 
#' 2011-12-19: moved from hsLibFingerprint.r
#' 
#' @param moniPoint Name of monitoring point, e.g. "TEG", "STA", "MUE"
#' @return Last wavelength being provided by spectrometer at given monitoring
#'   point
hsLastWL <- function(moniPoint)
{
  lastWL <- ifelse(
    moniPoint == "TEG", 750, ifelse(
      moniPoint == "STA", 740, ifelse(
        moniPoint == "MUE", 732.5, 0
      )
    )
  )
  
  if (lastWL == 0) {
    print(paste("Unknown monitoring point:", moniPoint))
  }
  
  lastWL
}

# hsWavelengthAtIndex ----------------------------------------------------------

#' Returns the wavelength in nm that belongs to the given column index i.
#' 
#' 2011-12-19: moved from hsLibFingerprint.r
#' 
#' @param i Index (i = 1: 200nm, i = 2: 202.5nm, ..., i = 217: 740nm)
#' @return Wavelength corresponding to index
hsWavelengthAtIndex <- function(i)
{
  200 + (i - 1) * 2.5
}

# hsIndexOfWavelength ----------------------------------------------------------

#' Index of Wavelength
#' 
#' Returns the index at which the wavelength given in nm can be found in a
#' vector (i = 1: 200nm, i = 2: 202.5nm, ..., i = 217: 740nm). 
#' 
#' 2011-12-19: moved from hsLibFingerprint.r
#' 
#' @param wavelength Wavelength for which corresponding index shall be
#'   determined
#' @return Index corresponding the given wavelength
hsIndexOfWavelength <- function(wavelength) 
{
  (wavelength - 200) / 2.5 + 1
}
