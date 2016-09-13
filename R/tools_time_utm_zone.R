#' Find the generic timezone of longitude coordinates
#'
#' \code{get_tz} finds the GMT+X timezone corresponding to the longitude
#' locations given.
#' @param longitude the longitude of location
#' @return a string with corresponding timezone. Beware of sign inversion :
#'   "Etc/GMT+2" is in reality GMT-2
#' @examples
#' get_tz(150)
#' @export

get_tz <- Vectorize(function(longitude) {
  if (longitude > 180)
    longitude <- longitude - 360
  lag = (longitude + 7.5) %/% 15
  if (lag < 0) {
    tz <- paste("Etc/GMT+", -lag, sep = "")
  } else {
    tz <- paste("Etc/GMT-", lag, sep = "")
  }
  return(tz)
})

#' Find the UTM zone of latitude-longitude coordinates
#'
#' \code{UTM_zone} finds the UTM timezone corresponding to the longitude
#' locations given. Modified from Cyril Bernard.
#' @param latitude the latitude of location
#' @param longitude the longitude of location
#' @return a vector which first element is the zone and second the hemisphere
#' @examples
#' UTM_zone(45,12)
#' @export

UTM_zone <- Vectorize(function(latitude,longitude) {
  meanLON <- longitude + 180
  meanLAT <- latitude
  n_zone <- as.character(ceiling(meanLON / 6))
  hemi <- ifelse(meanLAT < 0,'S','Na')
  return(c(n_zone, hemi))
})

#' Generate CRS string for the UTM zone corresponding to latitude-longitude
#' coordinates
#'
#' \code{CRS_UTM_zone} finds the UTM timezone corresponding to the longitude
#' locations given and generates the CRS string corresponding. Modified from
#' Cyril Bernard.
#' @param latitude the latitude of location
#' @param longitude the longitude of location
#' @return a CRS string
#' @examples
#' CRS_UTM_zone(45,12)
#' @export


CRS_UTM_zone <- Vectorize(function(latitude, longitude) {
  utmz <- UTM_zone(latitude, longitude)
  crs_utm <-
    paste0(
      '+proj=utm +zone=',
      utmz[1],
      ifelse(utmz[2] == 'S', ' +south', ''),
      ' +datum=WGS84 +units=m +no_defs'
    )
})

#' Convert POSIXct objects to a new timezone
#'
#' \code{changeTZ} change the timezone of a POSIXct objects
#' @param x a POSIXct object
#' @param newTZ a valid timezone
#' @return a POSIXct objects with datetime in newTZ timezone
#' @examples
#' changeTZ(x,"Africa/Harare")
#' @export

changeTZ <- function(x,newTZ){
  return(
    as.POSIXct(strptime(
      format(x, tz = newTZ),
      "%Y-%m-%d %H:%M:%S",
      tz = newTZ
    ))
  )
}


#' Get Julian Day
#'
#' \code{get_julian} calculate julian day of a POSIXct object.
#' @param x a POSIXct object
#' @param origin facultative date object for origin of julian days. Default January, 1st 2000.
#' @return a numeric object with days passed since origin
#' @examples
#' changeTZ(x,"Africa/Harare")
#' @export

get_julian <- function(x, origin = NULL) {
  if (is.null(origin)) {
    TZ <- attr(x,"tzone")
    origin =  as.POSIXct(strptime(
      paste("2000", "-01-01 00:00:01", sep = ""),
      "%Y-%m-%d %H:%M:%S"
    ), tz = TZ)
  }
  julday = floor(julian(x,origin=origin))
  julday = as.numeric(julday)
  return(julday)
}


