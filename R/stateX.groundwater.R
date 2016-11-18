#' stateX.layers
#' The function \code{stateX.groundwater} updates the saturation levels.
#' It computes the runoff event that occurs in each of layer, shift the layer by one timestep and then update the layer value with the runoff value
#' @param NoL number of level zone
#' @param Layers matrix describing the groundwater; the number of column represent the delay-steps, the row represent the levels (1st is the fastest, the last is the slowest)
#' @param ddist states (in mm) of each saturation level
#' @param X xcess water
#' @param layerUH Unit Hydrograph of the layer
#' @param nbStepsDelay delay-steps of each layer
#' @param Magkap Magkap
#' @param M  Groundwater Storage Capacity (GSC)
#' @return The output is a matrix describing the groundwater.
#'  the number of column represent the delay-steps, the row represent the levels (1st is the fastest, the last is the slowest)
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' stateX.layers()
#' }
stateX.groundwater <-function(NoL,Layers,ddist,X,layerUH,nbStepsDelay,Magkap,M){

  for (j in 1:NoL){
    # now-event to be updated
    qlayer <-ddist[j]*X*layerUH[j,]

    # shifting every layer one timestep ahead
    if(nbStepsDelay[j] >1){
      Layers[j,(1:nbStepsDelay[j]-1)] <- Layers[j,2:nbStepsDelay[j]]
      Layers[j,nbStepsDelay[j]] <-0.0
      Layers[j,1:nbStepsDelay[j]] <- Layers[j,1:nbStepsDelay[j]] + qlayer[1:nbStepsDelay[j]]
    } else if(nbStepsDelay[j]==1) {
      Layers[j,1:nbStepsDelay[j]] <- qlayer
    }
  }
  res <- list(Magkap = Magkap,
              M      = M,
              Layers = Layers)

  return(res)
}
