#' grd.M
#'
#' The function \code{grd.M()} computes the Groundwater Storage Capacity (GSC)
#' @param NoL number of Layers
#' @param Res_prob groundwater saturation quantiles of each level
#' @keywords groundwater
#' @return The output is a scalar
#' @export
#' @examples
#' \dontrun{
#' grd.M()
#' }

grd.M <-function(NoL,Res_prob){
  res <- Res_prob[(NoL-1)]
  return(res)
}
