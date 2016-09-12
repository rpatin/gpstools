#' timeline class description
#'
#' @param x a \code{timeline} object generated for instance by
#'   \code{\link{create_timeline}}
#' @name timeline
NULL

#' Print object of \code{timeline} class
#'
#' @rdname timeline
#' @export


print.timeline <- function(x){
  nInd <- length(unique(x$main$id))
  totalLength <- as.numeric(difftime(max(x$main$endPeriod),min(x$main$startPeriod),units = "days"))

  duration <-  as.numeric(difftime(x$main$endPeriod,x$main$startPeriod,units = "days"))
  minLength <- min(duration)
  maxLength <- max(duration)
  medLength <- median(duration)

  cat(paste("Movement Data : ",
            "\n Total Duration : ", printday(totalLength),
            "\n Number of Individuals : ", nInd,
            "\n Number of Burst : ", nrow(x$main),
            "\n Minimum Burst Duration : ", printday(minLength),
            "\n Median Burst Duration : ", printday(medLength),
            "\n Maximum Burst Duration : ", printday(maxLength),
            "\n Number of bins : ",x$maxdist/x$binsize,sep=""))
  if(!is.data.frame(x$activity))
  {
    cat("\n No Activity Data")
  } else {
    nInd <- length(unique(x$activity$id))
    totalLength <- as.numeric(difftime(max(x$activity$endPeriod),min(x$activity$startPeriod),units = "days"))

    duration <-  as.numeric(difftime(x$activity$endPeriod,x$activity$startPeriod,units = "days"))
    minLength <- min(duration)
    maxLength <- max(duration)
    medLength <- median(duration)

    cat(paste("\n Activity Data : ",
              "\n Total Duration : ", printday(totalLength),
              "\n Number of Individuals : ", nInd,
              "\n Minimum Burst Duration : ", printday(minLength),
              "\n Median Burst Duration : ", printday(medLength),
              "\n Maximum Burst Duration : ", printday(maxLength),
              "\n Number of bins : ",x$maxdist/x$binsize,sep=""))
  }
}


#' Plot object of \code{timeline} class
#' @rdname timeline
#' @export

#  x <- test.explo
plot.timeline <- function(x,theme=NULL) {
  out2  <- x$main
  out.act2  <- x$activity


  g <- ggplot2::ggplot(out2)

  if( is.data.frame(out.act2) ){
    # dummy plot of a small ribbon to add activity legend
    ribbon <- data.frame(x=c(min(out2$startPeriod),min(out2$startPeriod)+1),ymin=c(1,1),ymax=c(1.01,1.01))
    g <- g+ggplot2::geom_ribbon(data=ribbon,ggplot2::aes(x=x,ymin=ymin,ymax=ymax,fill='orange')) +
      ggplot2::scale_fill_manual(name = NULL,values =c('orange'='orange'), labels = c('Activity'))


   g <- g + ggplot2::geom_segment(data=out.act2,ggplot2::aes(x=startPeriod, xend=endPeriod, y=ybis-.1, yend=ybis-.1),col='orange', size=3)#+  ggplot2::guides(colour =  ggplot2::guide_legend(override.aes = list(size=3)))
  }

  if( length(unique(out2$burst)) == 1 ){
    g <- g +  ggplot2::geom_segment(ggplot2::aes(x=startPeriod, xend=endPeriod, y=ybis+.1, yend=ybis+.1),col='grey40', size=2.5)
  } else {
    g <- g +  ggplot2::geom_segment(ggplot2::aes(x=startPeriod, xend=endPeriod, y=ybis+.1, yend=ybis+.1,col=burst), size=2.5)
    }

   g <- g+ggplot2::xlab("Time") + ggplot2::ylab("Animal IDs")+ ggplot2::scale_y_continuous(labels = x$idplots,breaks=1:max(out2$ybis))

  if(is.null(theme)) {
    g <- g + ggplot2::theme_bw()
  } else {
    g <- g + theme
  }
  return(g)
}
