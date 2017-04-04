#' coords
#'
#' coords
#' @param obj spatial object
#' @param indice indice of the object in case of multi-object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' coords(...)
#' }
coords <- function(obj,indice=NULL) {
  type <- class(obj)[1]
  res <- switch(type,
    "SpatialLines"             = coords.SpatialLines(obj=obj, indice=indice),
    "SpatialLinesDataFrame"    = coords.SpatialLines(obj=obj, indice=indice),
    "SpatialPolygons"          = coords.SpatialPolygons(obj=obj, indice=indice),
    "SpatialPolygonsDataFrame" = coords.SpatialPolygonsDataFrame(obj=obj, indice=indice),
    "SpatialPoints"            = coords.SpatialPoints(obj=obj, indice=indice),
    "SpatialGrid"              = coords.SpatialGrid(obj=obj, indice=indice),
    "SpatialGridDataFrame"     = coords.SpatialGrid(obj=obj, indice=indice),
    stop("Object type not recognized: ", type))

}

coords.SpatialLines <- function(obj,indice=NULL){
  if (is.null(indice)){
     res <- obj@Lines[[1]]@coords
  } else {
     res <- obj@lines[[indice]]@Lines[[1]]@coords
  }
  return(res)
}

coords.SpatialPolygons <- function(obj,indice=NULL){
  if (is.null(indice)){
     res <- obj@Polygons[[1]]@coords
  } else {
     res <- obj@polygons[[indice]]@Polygons[[1]]@coords
  }
  return(res)
}

coords.SpatialPolygonsDataFrame <- function(obj,indice=NULL){
  if (is.null(indice)){
     res <- obj@Polygons[[1]]@coords
  } else {
     res <- obj@polygons[[indice]]@Polygons[[1]]@coords
  }
  return(res)
}

coords.SpatialPoints <- function(obj,indice=NULL){
  if (is.null(indice)){
     res <- obj@coords
  } else {
     res <- obj@coords[indice,]
  }
  return(res)
}

coords.SpatialGrid <- function(obj,indice=NULL){
  if (is.null(indice)){
     res <- sp::coordinates(obj)
  } else {
     res <- sp::coordinates(obj)[indice,]
  }
  return(res)
}
