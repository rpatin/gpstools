#' Creates a timeline for summarizing movement database
#'
#' \code{timeline} summarize for each individuals the duration of data available.
#'
#' @param df a data frame with data and a column in POSIXct
#' @param burstcol column names with defined burst
#' @param idcol column names for id
#' @param timecol column names for dateTime
#' @param color column names for the colors of data. If NULL no colors are added.
#' @param actdf a dataframe (optionnal) for adding activity duration. Must contains idcol and burstcol.
#' @param acttimecol colname for dateTime in actdf
#' @param sep maximum time between two locations to consider them in the same sequence
#' @param unit time unit of sep
#' @return a \code{\link{timeline}}
#' @examples
#' data("hwzebra")
#' timeline(hwzebra)
#' @export
#
# data("hwzebra")
# df <- hwzebra;rm(hwzebra)
# actdf <- read.csv("../../These/Hwange/Zebra - Data/data/Formatted/activity_2016_08_18.csv.gz")
# actdf <-
#   mutate(actdf, dateTime = as.POSIXct(strptime(paste(dateTime), "%Y-%m-%d %H:%M:%S", tz =
#                                                "Africa/Harare")))
#
# burstcol="burstname"
# idcol="id"
# timecol="expectTime"
# color="burst"
# # actdf=NULL
# acttimecol="dateTime"
# sep=7;unit='days'
# x <- create_timeline(df=df,actdf=actdf,color="burst")

# load("../workspace_timeline.rda")

timeline <- function(df,burstcol="burstname",idcol="id",timecol="expectTime",color=NULL,actdf=NULL,acttimecol="dateTime",sep=7,unit='days') {

  evalstr <- paste("df <- dplyr::arrange(df,",idcol,",",burstcol,",",timecol,")",sep="")
  eval(parse(text=evalstr))

  if(is.null(color)){
    df$color <- 1
    color = "color"
  }
  out <- eval(parse(text=paste("plyr::ddply(df,~",burstcol,",function(x){
        extractPeriods(
          x[,timecol],
          id = dplyr::first(x[, idcol]),
          burst = dplyr::first(x[, color]),
          sep = sep,
          unit = unit
        )
    })",sep="")))
  out.split <- split(out,out[,idcol])
  sorting <- lapply(out.split,function(x){x <- dplyr::arrange(x,startPeriod)
  return(data.frame(id=dplyr::first(x[,idcol]),start=dplyr::first(x$startPeriod)))})
  sort <- do.call('rbind',sorting)
  sort <- dplyr::arrange(sort,start)
  sort$y <- GetLetter(nrow(sort))

  sort <- dplyr::arrange(sort,y)
  sort <- dplyr::mutate(sort,ybis=1:nrow(sort))

  out2 <- dplyr::left_join(out,sort,by=c('id'))
  out2 <- dplyr::mutate(out2,plotid = paste(y,id,sep="-"))
  # levels(out$id)<- as.character(sort$id)
  out2 <- dplyr::arrange(out2,plotid)


  if (!is.null(actdf)) {
    out.act <- eval(parse(text=paste("plyr::ddply(actdf,~",idcol,",function(x){
        extractPeriods(
          x[,acttimecol],
          id = dplyr::first(x[, idcol]),
          sep = sep,
          unit = unit
        )
    })",sep="")))
    out.act2 <- dplyr::left_join(out.act,sort,by=c("id"))
    out.act2 <- dplyr::mutate(out.act2,plotid = paste(y,id,sep="-"))
    out.act2 <- dplyr::arrange(out.act2,plotid)
  } else {
    out.act2 <- NA
  }
  output <- list("main"=out2,
                 "activity"=out.act2)
  class(output) <- "timeline"
  return(output)
}





