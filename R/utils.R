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
