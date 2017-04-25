#' indice_subspatial
#'
#' indice_subspatial
#' @param spatial spatial grid S4 object
#' @param s sub spatial S4 object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' indice_subspatial()
#' }
indice_subspatial <- function(spatial,s) {

  stopifnot(inherits(spatial,"SpatialGrid") || inherits(spatial,"SpatialGridDataFrame"))

  if (!identical(spatial,s)){
    # get topology of spatial
    gridTopo <- buildGridTopo(spatial)

    # spatial2grid
    if (!inherits(s,"SpatialGrid") || inherits(s,"SpatialGridDataFrame")) {
       sRect <- spatial2grid(obj=s,cellsize=gridTopo$cellsize)
    } else sRect <- s


    # indice main-domain
    indice_main <- indice(spatial1=sRect,spatial2=spatial)

    # indice sub-domain
    indice_sub <- indice(spatial1=spatial,spatial2=sRect)

    # indice_crop
    if (!inherits(s,"SpatialGrid") || inherits(s,"SpatialGridDataFrame")) {
       indice_sub_crop <- crop(spatial=sRect,s=s)
    } else {
      indice_sub_crop <- list(cropX = numeric(0),
                              cropY = numeric(0),
                              cropXY = numeric(0))
    }

    return(list(indice_main = indice_main,
                indice_sub = c(indice_sub,indice_sub_crop)
                ))
  } else {
    gridTopo <- buildGridTopo(spatial)
    return(list(indice_main = list(indiceX = list(offset=1,count=gridTopo$cells.dim[1]),
                                   indiceY = list(offset=1,count=gridTopo$cells.dim[2])),
                indice_sub = list(indiceX = list(offset=1,count=gridTopo$cells.dim[1]),
                                  indiceY = list(offset=1,count=gridTopo$cells.dim[2]),
                                  cropX = numeric(0),
                                  cropY = numeric(0),
                                  cropXY = numeric(0))
                ))
  }

}

indice <- function(spatial1,spatial2){
  # dim Info
  gridTopo <- buildGridTopo(spatial2)

  # spatial indice
  tmp  <- contain(spatial1,spatial2)
  indice_spatial <- vec2mat(tmp,Nx=gridTopo$cells.dim[1],Ny=gridTopo$cells.dim[2])

  indiceX <- offsetCount(indice_spatial$indice_x)
  indiceY <- offsetCount(indice_spatial$indice_y)

  return(list(indiceX = indiceX,
              indiceY = indiceY))
}

crop <- function(spatial,s){
  gridTopo <- buildGridTopo(spatial)
  indice_spatial2 <- vec2mat(spatialManip::contain(s,spatial,REVERSE=TRUE),Nx=gridTopo$cells.dim[1],Ny=gridTopo$cells.dim[2])
  cropX <- indice_spatial2$indice_x
  cropY <- indice_spatial2$indice_y
  cropXY <- mat2vec(cropX,cropY,Nx=gridTopo$cells.dim[1],Ny=gridTopo$cells.dim[2])$indice_xy
  return(list(cropX = cropX,
              cropY = cropY,
              cropXY = cropXY))
}

#' mat2vec
#'
#' mat2vec
#' @param indice_x indice_x
#' @param indice_y indice_y
#' @param Nx Nx
#' @param Ny Ny
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' mat2vec()
#' }
mat2vec <- function(indice_x,indice_y,Nx,Ny) {
   indice_xy <- (indice_y-1)*Nx+indice_x
   res <- list(indice_xy=indice_xy)
 return(res)
}

#' vec2mat
#'
#' vec2mat
#' @param indice indice
#' @param Nx Nx
#' @param Ny Ny
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' vec2mat()
#' }
vec2mat <- function(indice,Nx,Ny) {
     indice_y <- trunc((indice-1)/Nx+1)
     indice_x <- trunc(indice-(indice_y-1)*Nx)
     res <- list(indice_x=indice_x,indice_y=indice_y)
     return(res)
}

offsetCount <- function(ind){
  offset = min(ind)
  count  = max(ind)-min(ind)+1
  return(list(offset=offset,count=count))
}
