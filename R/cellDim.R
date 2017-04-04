#' cellDim
#'
#' cellDim
#' @param obj spatial object
#' @param cellsize cellsize
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' cellDim(...)
#' }
cellDim <- function(obj,cellsize,...) {
  type <- class(obj)[1]
  res <- switch(type,
    "SpatialLines"             = cellDim.SpatialLines(obj=obj, cellsize=cellsize,...),
    "SpatialLinesDataFrame"    = cellDim.SpatialLines(obj=obj, cellsize=cellsize,...),
    "SpatialPolygons"          = cellDim.SpatialPolygons(obj=obj, cellsize=cellsize, ...),
    "SpatialPolygonsDataFrame" = cellDim.SpatialPolygonsDataFrame(obj=obj, cellsize=cellsize,...),
    "SpatialPoints"            = cellDim.SpatialPoints(obj=obj, cellsize=cellsize),
    "SpatialGrid"              = cellDim.SpatialGrid(obj=obj, cellsize=cellsize),
    "SpatialGridDataFrame"     = cellDim.SpatialGrid(obj=obj, cellsize=cellsize),
    stop("Object type not recognized: ", type))

  return(res)

}

cellDim.SpatialLines <- function(obj,cellsize,indice=NULL) {
  tmp <- minmax(obj,indice)
  res <- round((tmp[,2]-tmp[,1])/cellsize+1)
  return(res)
}

cellDim.SpatialPolygons <- function(obj,cellsize,indice=NULL) {
  tmp <- minmax(obj,indice)
  res <- round((tmp[,2]-tmp[,1])/cellsize+1)
  return(res)
}

cellDim.SpatialPolygonsDataFrame <- function(obj,cellsize,indice=NULL) {
  tmp <- minmax(obj,indice)
  res <- round((tmp[,2]-tmp[,1])/cellsize+1)
  return(res)
}

cellDim.SpatialPoints <- function(obj,cellsize) {
  tmp <- minmax(obj)
  res <- round((tmp[,2]-tmp[,1])/cellsize+1)
  return(res)
}

cellDim.SpatialGrid <- function(obj,cellsize) {
  tmp <- minmax(obj)
  res <- round((tmp[,2]-tmp[,1])/cellsize)
  return(res)
}
