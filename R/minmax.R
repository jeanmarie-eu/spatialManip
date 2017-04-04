#' minmax
#'
#' minmax
#' @param obj spatial object
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' minmax(...)
#' }
minmax <- function(obj,...) {
  type <- class(obj)[1]
  res <- switch(type,
    "SpatialLines"             = minmax.SpatialLines(obj=obj,...),
    "SpatialLinesDataFrame"    = minmax.SpatialLines(obj=obj,...),
    "SpatialPolygons"          = minmax.SpatialPolygons(obj=obj,...),
    "SpatialPolygonsDataFrame" = minmax.SpatialPolygonsDataFrame(obj=obj,...),
    "SpatialPoints"            = minmax.SpatialPoints(obj=obj,...),
    "SpatialGrid"              = minmax.SpatialGrid(obj=obj,...),
    "SpatialGridDataFrame"     = minmax.SpatialGridDataFrame(obj=obj,...),
    stop("Object type not recognized: ", type))

  return(res)

}

minmax.SpatialLines <- function(obj,indice=NULL){
  if (is.null(indice)){
     minmax <- sp::bbox(obj)
  } else {
     minmax <- sp::bbox(obj@lines[[indice]])
  }
  return(minmax)
}

minmax.SpatialPolygons <- function(obj,indice=NULL){
  if (is.null(indice)){
     minmax <- sp::bbox(obj)
  } else {
     minmax <- sp::bbox(obj@polygons[[indice]])
  }
  return(minmax)
}

minmax.SpatialPolygonsDataFrame <- function(obj,indice=NULL){
  if (is.null(indice)){
     minmax <- sp::bbox(obj)
  } else {
     minmax <- sp::bbox(obj@polygons[[indice]])
  }
  return(minmax)
}

minmax.SpatialPoints <- function(obj){
  minmax <- sp::bbox(obj)
  return(minmax)
}

minmax.SpatialGrid <- function(obj){
  minmax <- sp::bbox(obj)
  return(minmax)
}

minmax.SpatialGridDataFrame <- function(obj){
  minmax <- sp::bbox(obj)
  return(minmax)
}
