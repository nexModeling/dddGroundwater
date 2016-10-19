#' grd.ddistx
#'
#' The function \code{grd.ddistx()} informs about the current capacity of each level (in mm)
#' @param Layers matrix describing the groundwater; the number of column represent the delay-steps, the row represent the levels (1st is the fastest, the last is the slowest)
#' @param Magkap Maximaum capacity
#' @param nbStepsDelay delay-steps for each level
#' @return The output is a vector giving the current capacity of each of the level
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' grd.ddistx()
#' }
grd.ddistx <-function(Layers,Magkap,nbStepsDelay){

  NoL <- length(Magkap)
  aktMag <- rep(0,NoL)
  ddistx <- rep(0,NoL)
  for (j in NoL:1) {# NOL is the slowest level 1 is the fastest
    #state after this timestep water is gone. amount of water  in mm, minus current timestep
    aktMag[j] <-sum(Layers[j,2:nbStepsDelay[j]])
    # ddistx informs on current capacity for each level in mm.
    if (aktMag[j] < Magkap[j]) ddistx[j] <- Magkap[j]- aktMag[j]
  }

  return(ddistx)
}
