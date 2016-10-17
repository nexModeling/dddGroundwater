#' grd.ddist
#'
#' The function \code{grd.ddist()} computes the states of each saturation level.
#' Unit is in millimeters.
#' @param X excess water value
#' @param ddistx distance distribution of each saturation level
#' @return The output is a vector giving the state of each of the level
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' grd.ddist()
#' }
grd.ddist <-function(X,ddistx){

  #reduction in input
  Xred <- X

  NoL <- length(ddistx)
  ddist <- rep(0,NoL)

  # Rem: NOL is the slowest level 1 is the fastest
  for (j in NoL:1){
    if(Xred > 0){
      differ <- ddistx[j]-Xred

      #if saturation deficit in level j is less than input
      if (differ < 0)  {
        #divided by original X. Tells us the fraction of input needed for this level
        ddist[j] <-ddistx[j]/X
        Xred <- Xred-ddistx[j]# input is reduced correspondingly
      } else if (differ >= 0) {
        if (j < NoL) ddist[j] <- 1 - sum(ddist[(j+1):NoL])
        if (j==NoL) ddist[j] <-1
        if(j >1) ddist[(j-1):1] <-0 # we allow for level 1 to receive the rest in case input is greater than capacity in levels
        Xred <- 0
      }
    }
  }

  return(ddist)
}
