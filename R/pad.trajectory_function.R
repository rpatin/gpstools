#' Regularize irregular trajects and/or subsample trajects
#'
#' \code{pad.trajectory} takes a matrix of locations associated to a vector of
#' times at which these locations were acquired, and return a pseudo-regular
#' times-series of locations filled with NAs when no locations were acquired
#' within a duration tol from an expected time. When several locations were
#' acquired within the duration \code{tol}, only the closest in time from the
#' expected time of acquisition is kept. Because of the way it does the job it
#' will also  keep only one of several duplicates. This function was made
#' because it seems that \code{setNA} and \code{sett0} in adehabitatLT want ALL
#' acquired locations to be in the output trajectory - this is problematic for
#' some tracks which were very irregulars. Note that this function can be useful
#' to subsample trajectories.

#' @param xy matrix of locations
#' @param date.ref a POSIXct object defining the date/time at which the output
#'   trajectory should start
#' @param dt the duration (minutes) of the interval between two locations.
#' @param tol the tolerance (minutes) within which a acquired location is close
#'   enough to be kept for locating the animal at an expected time
#' @param correction.xy, none if the real times of acquired locations should be
#'   kept, cs if the output trajectory should be made regular (see above)
#' @param returnltraj should the function return a ltraj object rather than a
#'   data frame
#' @param idtraj the id that will be used to identify the ltraj object
#' @param bursttraj the burst used to identify the ltraj object
#' @param Index corresponding to the line in the original dataframe i.e. used
#'   afterwards to retrieve  other attributes from the original data.
#' @param max_sampling if you want not to calculate all time differences
#' @return  a dataframe or ltraj object with theoretical hour of acquisition and
#'   real location.
#'
#' @examples
#' pad.trajectory(xy,time,date.ref,dt,tol,correction.xy="cs",returnltraj=TRUE,idtraj="Zebra12",bursttraj="Zebra12_1h",Index)
#' @export


# tmp <- pad.trajectory(xy = cbind(df$x,df$y),time = df$dateTime,date.ref = start,dt = sampling_rate,tol=tol,idtraj=first(df$id),bursttraj = first(df$burst),Index=df$Indexing,correction.xy = "none",max_sampling = 20) # Crée une sequence de temps régulière et assigne à chaque pas de temps le point GPS le plus proche <3min d'écart.


pad.trajectory <- function(xy, time, date.ref, dt, tol, correction.xy=c("none", "cs"), returnltraj=FALSE, idtraj=NULL, bursttraj=NULL, Index=NULL,max_sampling=NULL){

  if (isTRUE(returnltraj) & (is.null(idtraj) | is.null(bursttraj))) {
      cat("Please provide idtraj and burstraj, they are needed in order to return ltraj objects")
      return(NULL)
  }

  # create a vector of all times at which locations should have been acquired
  nint <- ceiling(as.numeric(difftime(tail(time,1), date.ref, units = "mins"))/dt)
  alltimes <- date.ref + (0:nint)*dt*60

  # create a vector of real times at which locations where acquired and a matrix for locations
  realtimes <- alltimes
  paddedxy <- matrix(NA, nint + 1,2)

  if (!is.null(Index)) {
    indices <- rep(NA,length(realtimes))
    # will fill this vector with Index arguments
  }

  # for each real location find the closest (in time) expected one within a tol tolerance
  # in this version of the function I made it (a bit) faster by not going through all alltimes
  s <- 1; n <- length(alltimes)
  if(!is.null(max_sampling)) {
    n <- min(s+max_sampling,length(alltimes))
    diffrealtime <- c(0,difftime(time[2:length(time)],time[1:(length(time)-1)],units="mins"))
  }

  for (i in 1:length(time)) {

    if(!is.null(max_sampling)) {
      n <- min(length(alltimes),n+ceiling(diffrealtime[i]/dt))
    }
    alldt <- abs(as.numeric(difftime(time[i], alltimes[s:n], units = "mins")))
    if (min(alldt) <= tol) {
      # is there an expected locations within tol from the real location?
      id <- which.min(alldt) + s - 1
      if (is.na(paddedxy[id, 1])) {
        # has another real point already be placed at this expected loc?
        realtimes[id] <-  time[i]
        paddedxy[id, ] <- xy[i, ] # if not, then place this one
        if (!is.null(Index)) {
          indices[id] <- Index[i]
          # and place corresponding Index
        }

      } else {
        # if yes,...
        if (abs(difftime(realtimes[id], alltimes[id], units = "mins")) >
            min(alldt)) {
          # is the former point closer in time?
          realtimes[id] <-
            time[i]
          paddedxy[id, ] <-
            xy[i, ]
        } # if not, place this one (if yes, keep the old one and do nothing
        if (!is.null(Index)) {
          indices[id] <- Index[i]
          # and place corresponding Index
        }
      }
      s <- id
      if(!is.null(max_sampling)) {n <- min(s+max_sampling,length(alltimes))}

    }
  }


  # are locations forced to be regularized with a correction assuming constant speed?
  if (correction.xy == "cs") {
    if (isTRUE(returnltraj)) {
      trajtemp <-
        adehabitatLT::as.ltraj(paddedxy, realtimes, id = idtraj, burst = bursttraj)
    } else {
      trajtemp <- adehabitatLT::as.ltraj(paddedxy, realtimes, id = "temp")
    }
    newtraj <-
      adehabitatLT::sett0(trajtemp,
                          date.ref=date.ref,
                          dt=dt,
                          correction.xy = "cs",
                          tol=150,
                          units = "min")
  }

  # return
  if (isTRUE(returnltraj)) {
    if (correction.xy == "cs") {
      return(newtraj)
    } else {
      return(adehabitatLT::as.ltraj(paddedxy, realtimes, id = idtraj, burst =
                                      bursttraj))
    }
  } else {
    if (correction.xy == "cs") {
      return(data.frame(
        expectTime = alltimes,
        realTime = alltimes,
        x = newtraj[[1]]$x,
        y = newtraj[[1]]$y
      ))
    } else {
      df <-
        data.frame(
          expectTime = alltimes,
          realTime = realtimes,
          x = paddedxy[, 1],
          y = paddedxy[, 2]
        )
      if (!is.null(Index)) {
        df$index <- indices
      }
      return(df)
    }
  }
}
