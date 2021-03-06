% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools_time_utm_zone.R
\name{UTM_zone}
\alias{UTM_zone}
\title{Find the UTM zone of latitude-longitude coordinates}
\usage{
UTM_zone(latitude, longitude)
}
\arguments{
\item{latitude}{the latitude of location}

\item{longitude}{the longitude of location}
}
\value{
a vector which first element is the zone and second the hemisphere
}
\description{
\code{UTM_zone} finds the UTM timezone corresponding to the longitude
locations given. Modified from Cyril Bernard.
}
\examples{
UTM_zone(45,12)
}

