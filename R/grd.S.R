#' grd.S
#'
#' The function \code{grd.S()} computes the deficit S for all sub surface layers (not the overland flow layer)
#'
#' It follows the expression \code{dS/dt = -dD/dt} from  Skaugen,Peerebom and Nilsson(2015)
#'
#' @param ddistx distance distribution of each saturation level
#' @param X excess water, scalar
#' @param ddist states (in mm) of each saturation level
#' @return The output is a scalar
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' grd.S()
#' }
grd.S <-function(ddistx,X,ddist){
  NoL<- length(ddistx)
  ddistxtemp <- ddistx-X*ddist                  # the deficit is reduced by input
  S <- (-1)*sum(ddistxtemp[2:NoL],na.rm=TRUE)   # This may be negative if input outstrips deficits
  return(S)
}
