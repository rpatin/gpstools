% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpol.trajectory_function.R
\name{interpol.trajectory}
\alias{interpol.trajectory}
\title{Regularize irregular trajects and/or subsample trajects}
\usage{
interpol.trajectory(xy, time, maxtime, nomove = FALSE)
}
\arguments{
\item{xy}{matrix of locations or NA values}

\item{time}{a POSIXct vector of date at which locations were acquired}

\item{maxtime}{maximum time interval between two locations beyond which interpolation is NOT done (minutes)}

\item{nomove}{in case two locations are exactly similar, should NA values in between by replaced by the  coordinates of this location?}
}
\description{
\code{interpol.trajectory} takes a matrix of locations associated to a vector
of times at which these locations were acquired, and return a dataframe with
the original and interpolated locations (identified in column "interp") ;
Author: S. Chamaillé-Jammes ; Last update: 22/05/2013
}
\examples{
interpol.trajectory(xy,time,maxtime=120,nomove=FALSE)
}

