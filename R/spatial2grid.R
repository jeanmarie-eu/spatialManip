#' spatial2grid
#'
#' spatial2grid
#' @param obj spatial object
#' @param cellsize cellsize
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' spatial2grid(...)
#' }
spatial2grid <- function(obj,cellsize=NULL,...) {
  x <- buildGridTopo(obj=obj,cellsize=cellsize,...)
  spdf <- construct(type="grid",x=x,proj4S=sp::CRS(sp::proj4string(obj)))
  return(spdf)
}
