#' crs
#'
#' crs
#' @param projargs projargs
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' crs()
#' }
crs <- function(projargs){
  res <- sp::CRS(projargs)
  return(res)
}

#' proj4string
#'
#' proj4string
#' @param projargs projargs
#' @keywords spatialManip
#' @export
#' @examples
#' \dontrun{
#' proj4string()
#' }
proj4string <- function(projargs){
  res <- sp::proj4string(projargs)
  return(res)
}
