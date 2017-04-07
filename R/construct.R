#' construct
#'
#' construct
#' @param type type of spatial object
#' @param x parameters to construct the a spatial object
#' @param proj4S pro4string
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' construct(...)
#' }
construct <- function(type,x,proj4S,...) {
  res <- switch(type,
    "lines"    = construct_lines(x=x,proj4S=proj4S,...),
    "polygons" = construct_polygons(x=x,proj4S=proj4S,...),
    "points"   = construct_points(x=x,proj4S=proj4S),
    "grid"     = construct_grid(x=x,proj4S=proj4S),
    (message=paste0("Invalid type:", type,".")))
  return(res)
}


construct_lines <- function(x,proj4S,name) {
   spdf <- sp::SpatialLines(list(sp::Lines(list(sp::Line(x)),name)),proj4string=proj4S)
   return(spdf)
}
construct_polygons <- function(x,proj4S,name) {
   spdf <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(x)),name)),proj4string=proj4S)
   return(spdf)
}

construct_points <- function(x,proj4S) {
   spdf <- sp::SpatialPoints(x,proj4string=proj4S)
   return(spdf)
}
construct_grid <- function(x,proj4S){
   grd <- sp::GridTopology(cellcentre.offset=x$cellcentre.offset, cellsize=x$cellsize, cells.dim=x$cells.dim)
   spdf <- sp::SpatialGrid(grd, proj4string=proj4S)
   return(spdf)
}
