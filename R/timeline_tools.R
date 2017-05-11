
#' Generate a sequence of letters
#'
#' \code{GetLetter} generates a sequence of 3 letter characters with a maximum
#' of 17576 elements
#' @param n number of element in the characters sequence
#' @return a vector of 3 letters characters
#' @examples
#' GetLetter(n)
#' @export

GetLetter = function(n){
  Let = LETTERS[1:26]
  df = expand.grid(A=Let,B=Let,C=Let)
  df = dplyr::mutate(df,ABC=paste(C,B,A,sep=""))
  return(df$ABC[1:n])
}

#' Extract periods in dateTime sequences
#'
#' \code{extractPeriods} extracts the different sequences of observations in a dateTime sequence.
#'
#' @param x POSIXct objects, a sequence of dateTime
#' @param sep maximum time of holes in date before introducing a new burst
#' @param unit unit of sep. Default "days"
#' @param id name of the sequence id
#' @param burst name of the sequence burst
#' @return a dataframe with all periods and their date of begin (startPeriod) and end (endPeriod)
#' @examples
#' extractPeriods(x)
#' @author Simon Chamaille-Jammes, \email{simon.chamaille@cefe.cnrs.fr}
#'
# sep =7
# unit='days'
# id= NULL
# burst=NULL
# subdf <- filter(df,id=="Nerina")
# subdf <- filter(subdf,!is.na(x))
# extractPeriods(subdf$expectTime,sep=3,unit="min")

extractPeriods <- function(x,sep=7,unit="days",id=NULL,burst=NULL){

  s <- which(difftime(x[-1],x[-length(x)],units="days")>sep)

  starts <- rep(NA,length(x)); starts[1] <- 1
  ends <- rep(NA,length(x)); ends[length(x)] <- 1

  if(length(s)>0){starts[s+1] <- 1; ends[s] <- 1}

  periods <- 1:sum(starts,na.rm=T)
  startPeriod <- x[which(starts==1)]; endPeriod <- x[which(ends==1)]

  returndf <- data.frame(periods,startPeriod,endPeriod)
  returndf$id <- id
  returndf$burst <- burst

  return(returndf)
}

# extractPeriods <- function(x,sep=7,unit="days",id=NULL,burst=NULL){
#
#   s <- which(difftime(x[-1],x[-length(x)],units=unit)>sep)
#
#   startper <- rep(NA,length(x))
#   endper <- rep(NA,length(x))
#   startper[1] <- "start"
#   endper[length(x)] <- "end"
#
#   if(length(s)>0){
#     endper[s] <- "end"
#     startper[s+1] <- "start"
#   }
#
#   periods <- 1:length(which(startper=="start"))
#   startPeriod <- x[which(startper=="start")]
#   endPeriod <- x[which(endper=="end")]
#
#   returndf <- data.frame(periods,startPeriod,endPeriod)
#   returndf$id <- id
#   returndf$burst <- burst
#   return(returndf)
# }

#' Transform a number of days into Years, Month, Day
#'
#' \code{printday} Transform a number of days into Years, Month, Day
#' @param n number of days
#' @return a character
#' @examples
#' printday(450)

printday = function(n){
  n <- floor(n)
  years <- n %/% 365
  month <- (n %% 365) %/% 31
  days  <- (n %% 365) %% 31

  output <- paste(years," years, ",month," months, ",days," days",sep="")
  return(output)
}
