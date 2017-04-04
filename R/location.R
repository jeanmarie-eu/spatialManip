#' location
#'
#' location
#' @param obj spatial object
#' @param points points
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' location(...)
#' }


location <- function(obj,points) {
  type <- class(obj)[1]
  res <- switch(type,
    "SpatialGrid"              = location.SpatialGrid(obj=obj, points=points),
    "SpatialGridDataFrame"     = location.SpatialGridDataFrame(obj=obj, points=points),
    stop("Object type not recognized: ", type))
  return(res)
}

location.SpatialGrid <- function(obj,points){
  results <- location_func(obj,points)
  return(results)
}

location.SpatialGridDataFrame <- function(obj,points){
  results <- location_func(obj,points)
  return(results)
}

location_func <- function(obj,points){
  points <- as.matrix(points)
  tmp <- sp::gridparameters(obj)
  cellsize <- tmp[1,2]
  xMinyMin <- cellcentre_offset(obj,cellsize)
  results<-list(indice_x=round((points[,1]-xMinyMin[1])/cellsize+1),
                indice_y=round((points[,2]-xMinyMin[2])/cellsize+1))
  return(results)
}
