#' xGridBuild
#'
#' xGridBuild
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#' @param cellsize cellsize
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' xGridBuild(...)
#' }
xGridBuild <- function(xmin,xmax,ymin,ymax,cellsize){
  dimX <- round((xmax-xmin)/cellsize)
  dimY <- round((ymax-ymin)/cellsize)
  x <- list(cellcentre.offset=c((xmin + cellsize/2), (ymin - cellsize/2)), cellsize=c(cellsize,cellsize), cells.dim=c(dimX,dimY))
  return(x)
}
