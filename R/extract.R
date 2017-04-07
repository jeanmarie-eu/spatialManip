#' extract
#'
#' extract
#' @param obj spatial object
#' @param indice indice of the object in case of multi-object
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' extract(...)
#' }

extract <- function(obj, indice=NULL,...) {
  type <- class(obj)[1]
  res <- switch(type,
    "SpatialLines"             = extract.SpatialLines(obj=obj, indice=indice,...),
    "SpatialLinesDataFrame"    = extract.SpatialLines(obj=obj, indice=indice,...),
    "SpatialPolygons"          = extract.SpatialPolygons(obj=obj, indice=indice,...),
    "SpatialPolygonsDataFrame" = extract.SpatialPolygons(obj=obj, indice=indice,...),
    "SpatialPoints"            = extract.SpatialPoints(obj=obj, indice=indice),
    "SpatialGrid"              = extract.SpatialGrid(obj=obj, indice=indice),
    "SpatialGridDataFrame"     = extract.SpatialGrid(obj=obj, indice=indice),
    stop("Object type not recognized: ", type))

  return(res)
}

extract.SpatialLines <- function(obj, indice=NULL, name=NULL){
  if (!is.null(indice)){
    spdf <- construct_lines(x=coords(obj=obj,indice=indice),name=name,proj4S=sp::CRS(sp::proj4string(obj)))
  } else spdf <- obj
  return(spdf)
}

extract.SpatialPolygons <- function(obj, indice=NULL, name=NULL){
  if (!is.null(indice)){
    spdf <- construct_polygons(x=coords(obj=obj,indice=indice),name=name,proj4S=sp::CRS(sp::proj4string(obj)))
  } else spdf <- obj
  return(spdf)
}

extract.SpatialPoints <- function(obj, indice=NULL){
  if (!is.null(indice)){
    spdf <- construct_points(x=coords(obj=obj,indice=indice),proj4S=sp::CRS(sp::proj4string(obj)))
  } else spdf <- obj
  return(spdf)
}

extract.SpatialGrid <- function(obj, indice=NULL){ #indice$offset=list(i,j),indice$count=list(i,j)
  if (!is.null(indice)){

    tmp <- buildGridTopo(obj)
    tmp2 <- list(
      cellcentre.offset = c(tmp$cellcentre.offset[1]+(indice$offset$i-1)*tmp$cellsize[1],tmp$cellcentre.offset$j+(indice$offset$j-1)*tmp$cellsize[2]),
      cellsize          = tmp$cellsize,
      cells.dim         = indice$count)

    spdf <- construct(type="grid",x=tmp2,proj4S=sp::CRS(sp::proj4string(obj)))
  } else spdf <- obj
  return(spdf)
}
