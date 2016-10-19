#' dddGroundwater.Magkap
#'
#' The function \code{grd.Magkap} computes the maximum capacity for each saturation level
#' @param NoL number of layer
#' @param Res_prob groundwater saturation quantiles for each level
#' @param CapacityUpperLevel capacity of the upper level
#' @return The output is a scalar
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' grd.Magkap()
#' }

grd.Magkap <-function(NoL,Res_prob,CapacityUpperLevel){

  ssRes1 <- rep (0,NoL)

  # capacity of overland flow level
  ssRes1[1] <-CapacityUpperLevel

  for (i in 2:(NoL-1)){
    ssRes1[i] <-Res_prob[NoL-i+1]-Res_prob[(NoL-i)]
  }

  # capacity for the first slowest level
  ssRes1[NoL] <- Res_prob[1]

  Magkap <- ssRes1[1:NoL]
  return(Magkap)
}
