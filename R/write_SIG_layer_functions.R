#' Write points and lines shapefiles for movement data/
#'
#' \code{write_shapefiles} takes a dataframe with movement data from several
#' animals, splits it into the requested burst and write 2 shapefiles for each
#' burst. One with points and one with lines.

#' @param df data.frame with movement data
#' @param coord.x1 first coordinates
#' @param coord.x2 second coordinates
#' @param proj4string CRS projection string for coordinates
#' @param burst colnames for separating the different files to write
#' @param dir directory for writing the files
#' @param KML if true also write KML files into a KML subdirectory
#' @param verbose if true print burst progress
#'
#' @examples
#' write_shapefiles(df,"SIG/")
#' @export


write_shapefiles <- function(df, dir, KML = FALSE, coord.x1 = "x", coord.x2 = "y", proj4string = "+proj=longlat +datum=WGS84 +no_defs", burst = "id",verbose=F){


  for (INDIV in unique(df[, burst])) {
    if(verbose){ message(paste("\n",INDIV,"\n",sep=""))}
    subdf <-
      eval(parse(text = paste(
        "dplyr::filter(df,!is.na(", coord.x1, "),
        ",burst," == INDIV)",sep = "")))

      stock <- subdf
      # write points shapefile
      sp::coordinates(subdf) <- c(coord.x1, coord.x2)
      sp::proj4string(subdf) <- proj4string

      rgdal::writeOGR(
        subdf,
        dsn = paste(dir, "/points_", INDIV, ".shp", sep = ""),
        layer = paste("points_", INDIV, sep = ""),
        driver = 'ESRI Shapefile',
        overwrite_layer = T
      )


      stock$idline <- 1:nrow(stock)
      LinesUTM <- lapply(1:(nrow(stock) - 1), function(j) {
        z <- stock[c(j, j + 1), ]
        return(sp::Lines(list(sp::Line(
          cbind(z[,coord.x1], z[,coord.x2])
        )), ID = dplyr::first(z$idline)))
      })
      rownames(stock) <- stock$idline
      Spline <-
        sp::SpatialLines(LinesUTM, proj4string = sp::CRS(proj4string))
      SpdfLine <-
        sp::SpatialLinesDataFrame(Spline, stock, match.ID = T)

      rgdal::writeOGR(
        SpdfLine,
        dsn = paste(dir,"/lines_", INDIV, ".shp", sep = ""),
        layer = paste("lines_", INDIV, sep = ""),
        driver = 'ESRI Shapefile',
        overwrite_layer = T)

      if(KML){
        if(dir.exists(paste(dir,"/KML"))){
          dir.create(paste(dir,"/KML"))

          rgdal::writeOGR(
            subdf,
            dsn = paste(dir, "/KML/points_", INDIV, ".kml", sep = ""),
            layer = paste("points_", INDIV, sep = ""),
            driver = 'KML',
            overwrite_layer = T
          )

          rgdal::writeOGR(
            SpdfLine,
            dsn = paste(dir,"/KML/lines_", INDIV, ".kml", sep = ""),
            layer = paste("lines_", INDIV, sep = ""),
            driver = 'KML',
            overwrite_layer = T)
        }
      }
  }
  return(invisible(NULL))
}
