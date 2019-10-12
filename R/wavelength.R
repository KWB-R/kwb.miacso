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
