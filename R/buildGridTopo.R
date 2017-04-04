#' buildGridTopo
#'
#' buildGridTopo
#' @param obj spatial object
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' buildGridTopo(...)
#' }
buildGridTopo <- function(obj,...) {
  type <- class(obj)[1]
  res <- switch(type,
    "SpatialLines"             = buildGridTopo.SpatialLines(obj=obj,...),
    "SpatialLinesDataFrame"    = buildGridTopo.SpatialLines(obj=obj,...),
    "SpatialPolygons"          = buildGridTopo.SpatialPolygons(obj=obj,...),
    "SpatialPolygonsDataFrame" = buildGridTopo.SpatialPolygonsDataFrame(obj=obj,...),
    "SpatialPoints"            = buildGridTopo.SpatialPoints(obj=obj,...),
    "SpatialGrid"              = buildGridTopo.SpatialGrid(obj=obj,...),
    "SpatialGridDataFrame"     = buildGridTopo.SpatialGrid(obj=obj,...),
    stop("Object type not recognized: ", type))

  return(res)
}

buildGridTopo.SpatialLines <- function(obj,indice=NULL,cellsize){
  res <- list(cellcentre.offset = cellcentre_offset(obj,cellsize,indice),
               cellsize         = cellsize,
               cells.dim        = cellDim(obj,cellsize))
  return(res)
}

buildGridTopo.SpatialPolygons <- function(obj,indice=NULL,cellsize){
  res <- list(cellcentre.offset = cellcentre_offset(obj,cellsize,indice),
               cellsize         = cellsize,
               cells.dim        = cellDim(obj,cellsize))
  return(res)
}

buildGridTopo.SpatialPolygonsDataFrame <- function(obj,indice=NULL,cellsize){
  res <- list(cellcentre.offset = cellcentre_offset(obj,cellsize,indice),
               cellsize         = cellsize,
               cells.dim        = cellDim(obj,cellsize))
  return(res)
}

buildGridTopo.SpatialPoints <- function(obj,cellsize){
  res <- list(cellcentre.offset = cellcentre_offset(obj,cellsize),
               cellsize         = cellsize,
               cells.dim        = cellDim(obj,cellsize))
  return(res)
}

buildGridTopo.SpatialGrid <- function(obj,cellsize=NULL){

  if (!is.null(cellsize)) {
    res <- list(cellcentre.offset = cellcentre_offset(obj,cellsize),
                 cellsize         = cellsize,
                 cells.dim        = cellDim(obj,cellsize))
  } else {
    tmp <- sp::getGridTopology(obj)
    res <- list(cellcentre.offset = tmp@cellcentre.offset,
                 cellsize         = tmp@cellsize,
                 cells.dim        = tmp@cells.dim)
  }
  return(res)
}
