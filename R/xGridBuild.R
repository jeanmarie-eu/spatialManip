#' xGridBuild
#'
#' xGridBuild
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param cellsize cellsize
#' @param CELLCENTER boolean
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' xGridBuild(...)
#' }
xGridBuild <- function(xmin,xmax,ymin,ymax,cellsize,CELLCENTER=FALSE){
  if (CELLCENTER) {
    xmin <- xmin - (cellsize/2)
    xmax <- xmax + (cellsize/2)
    ymin <- ymin - (cellsize/2)
    ymax <- ymax + (cellsize/2)
  }
  dimX <- round((xmax-xmin)/cellsize)
  dimY <- round((ymax-ymin)/cellsize)
  x <- list(cellcentre.offset=c((xmin + cellsize/2), (ymin + cellsize/2)), cellsize=c(cellsize,cellsize), cells.dim=c(dimX,dimY))
  return(x)
}
