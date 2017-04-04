#' contain
#'
#' contain
#' @param a spatial object
#' @param b spatial object
#' @param REVERSE boolean i.e not containing
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' contain()
#' }
contain <- function(a,b,REVERSE=FALSE){
  indice <- sp::over(a,b,returnList = TRUE)
  tmp <- seq(1,length(b))
  if (REVERSE) {
    res <- tmp[-unlist(indice)]
  } else {
    res <- tmp[unlist(indice)]
  }
  return(res)
}
