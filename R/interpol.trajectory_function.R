# Function interpol.trajectory
#
# The function returns a dataframe with the original and interpolated locations (identified in column "interp") # # Arguments:
# xy: matrix of locations or NA values
# time: a POSIXct vector of date at which locations were acquired
# maxtime: maximum time interval between two locations beyond which interpolation is NOT done (minutes)
# nomove: in case two locations are exactly similar, should NA values in between by replaced by the  coordinates of this location?
# Author: S. Chamaillé-Jammes
# Last update: 22/05/2013

#' Regularize irregular trajects and/or subsample trajects
#'
#' \code{interpol.trajectory} takes a matrix of locations associated to a vector
#' of times at which these locations were acquired, and return a dataframe with
#' the original and interpolated locations (identified in column "interp") ;
#' Author: S. Chamaillé-Jammes ; Last update: 22/05/2013

#' @param xy matrix of locations or NA values
#' @param time a POSIXct vector of date at which locations were acquired
#' @param maxtime maximum time interval between two locations beyond which interpolation is NOT done (minutes)
#' @param nomove in case two locations are exactly similar, should NA values in between by replaced by the  coordinates of this location?

#'
#' @examples
#' interpol.trajectory(xy,time,maxtime=120,nomove=FALSE)
#' @export

interpol.trajectory <- function(xy,time,maxtime,nomove=FALSE){

  # get ids of rows that have locations
  atlocs <- which(!is.na(xy[,1]))
  atlocsTime <- time[atlocs]
  ninter <- diff(atlocs)
  #diffTimes <- difftime(atlocsTime[-length(atlocsTime)],atlocsTime[-1],units="mins")
  # is the following one good ?
  diffTimes <- difftime(atlocsTime[-1],atlocsTime[-length(atlocsTime)],units="mins")

  # Create a vector storing if location is interpolated or not
  itp <- rep(NA,nrow(xy))
  itp[atlocs] <- "N"

  # do interpolation between those that are separated by less than or maxtime
  for(i in 1:(length(atlocs)-1)){
    #print(i)
    if(diffTimes[i]<=maxtime){
      xx <- xy[atlocs[c(i,i+1)],1]; yy <- xy[atlocs[c(i,i+1)],2]
      # check that the animal has moved and if so do according to nomove argument
      if(xx[1]==xx[2] | yy[1]==yy[2]){
        if(xx[1]==xx[2] & yy[1]==yy[2]){
          if(nomove){
            xy[atlocs[i]:atlocs[i+1],1] <- xx[1]
            xy[atlocs[i]:atlocs[i+1],2] <- yy[1]
          }
        } else {
          if(xx[1]==xx[2]){
            xy[atlocs[i]:atlocs[i+1],1] <- xx[1]
            xy[atlocs[i]:atlocs[i+1],2] <- yy[1]+(yy[2]-yy[1])/ninter[i]*(0:ninter[i])
          }
          if(yy[1]==yy[2]){
            xy[atlocs[i]:atlocs[i+1],1] <- xx[1]+(xx[2]-xx[1])/ninter[i]*(0:ninter[i])
            xy[atlocs[i]:atlocs[i+1],2] <- yy[1]
          }
        }
      } else {
        out <- approx(x=xx,y=yy,n=ninter[i]+1)
        # !! approx returns values ordered by x !!
        if(xy[atlocs[i],1]<xy[atlocs[i+1],1]){
          xy[atlocs[i]:atlocs[i+1],1] <- out$x
          xy[atlocs[i]:atlocs[i+1],2] <- out$y
        } else {
          xy[atlocs[i]:atlocs[i+1],1] <- rev(out$x)
          xy[atlocs[i]:atlocs[i+1],2] <- rev(out$y)
        }
      }}
  }

  # Update the vector storing if location is interpolated or not
  itp[is.na(itp) & !is.na(xy[,1])] <- "Y"

  # Return
  return(data.frame(x=xy[,1],y=xy[,2],expectTime=time,interp=itp))
}
