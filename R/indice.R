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
    sRect <- spatial2grid(obj=s,cellsize=gridTopo$cellsize)

    # spatial indice
    tmp  <- contain(sRect,spatial)
    indice_spatial <- vec2mat(tmp,Nx=gridTopo$cells.dim[1],Ny=gridTopo$cells.dim[2])

    #offset-count
    indiceX <- offsetCount(indice_spatial$indice_x)
    indiceY <- offsetCount(indice_spatial$indice_y)

    # non-contain spatial
    gridTopo <- buildGridTopo(sRect)
    indice_spatial2 <- vec2mat(spatialManip::contain(s,sRect,REVERSE=TRUE),Nx=gridTopo$cells.dim[1],Ny=gridTopo$cells.dim[2])
    cropX <- indice_spatial2$indice_x
    cropY <- indice_spatial2$indice_y
    cropXY <- mat2vec(cropX,cropY,Nx=gridTopo$cells.dim[1],Ny=gridTopo$cells.dim[2])$indice_xy

    return(list(indiceX = indiceX,
                indiceY = indiceY,
                cropX = cropX,
                cropY = cropY,
                cropXY = cropXY))
  } else {
    gridTopo <- buildGridTopo(spatial)
    return(list(indiceX = list(offset=1,count=gridTopo$cells.dim[1]),
                indiceY = list(offset=1,count=gridTopo$cells.dim[2]),
                cropX = numeric(0),
                cropY = numeric(0),
                cropXY = numeric(0)))
  }

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
