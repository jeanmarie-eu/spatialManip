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
  dimX <- round(((xmax-cellsize/2)-(xmin+cellsize/2))/cellsize)
  dimY <- round(((ymax-cellsize/2)-(ymin+cellsize/2))/cellsize)
  x <- list(cellcentre.offset=c((xmin + cellsize/2), (ymin - cellsize/2)), cellsize=c(cellsize,cellsize), cells.dim=c(dimX,dimY))
  return(x)
}
