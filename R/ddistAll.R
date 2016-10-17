#' ddistAll
#'
#' The function \code{ddistAll()} gathers three processes:
#' - it informs on current capacity for each level in mm.
#' - it computes the states (in mm) of each saturation level
#' - it computes the deficit S (for all sub surface layers, NOT overland flow layer)
#' @param Layers matrix describing the groundwater; the number of column represent the delay-steps, the row represent the levels (1st is the fastest, the last is the slowest)
#' @param Magkap Maximaum capacity
#' @param nbStepsDelay delay-steps for each level
#' @param X excess water value
#' @return The output is a list providing the three main features for groundwater( ddistx, ddist, S)
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' ddistAll()
#' }
ddistAll <-function(Layers,Magkap,nbStepsDelay,X){

  # States for each saturation level. Informs on current capacity for each level in mm.
  ddistxUpdate <- grd.ddistx(Layers=Layers,Magkap=Magkap,nbStepsDelay=nbStepsDelay)
  # OUTPUT: ddistx

  #
  ddistUpdate <- grd.ddist(X=X,ddistx=ddistxUpdate)
  # OUTPUT: ddist

  #updating the deficit: Sprim (for all sub surface layers, NOT overland flow layer)
  SUpdate <- grd.S(ddistx=ddistxUpdate,X=X,ddist=ddistUpdate)
  # OUTPUT: S

  res <- list(ddistx = ddistxUpdate,
              ddist  = ddistUpdate,
              S      = SUpdate)    #(-1)*D

  return(res)
}
