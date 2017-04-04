#' cellcentre_offset
#'
#' cellcentre_offset
#' @param obj spatial object
#' @param cellsize cellsize
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' cellcentre_offset(...)
#' }
cellcentre_offset <- function(obj,cellsize,...){
  res <- minmax(obj,...)[,1]+cellsize/2
  return(res)
}


#' cellcentre_range
#'
#' cellcentre_range
#' @param obj spatial object
#' @param cellsize cellsize
#' @param ... options according to the spatial object
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' cellcentre_range(...)
#' }
cellcentre_range <- function(obj,cellsize,...){
  res1 <- minmax(obj,...)[,1]+cellsize/2
  res2 <- minmax(obj,...)[,2]-cellsize/2
  return(rbind(res1,res2))
}
