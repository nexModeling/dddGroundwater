#' saturationSum
#'
#' the function \code{saturationSum()} computes the saturation sum
#' @param UHMAD Unit Hydrograph of the Mean Annual Discharge
#' @param MAD value of the Mean Annual Discharge
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param area in squared meters
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' saturationSum()
#' }
saturationSum <-function(UHMAD,MAD,Timeresinsec,area) {

  antBox <- length(UHMAD)

  sRes <-  rep(0,antBox) # saturation sum

  # Steady state Input eq. output in mm (x1000)
  StSt<- (1000*MAD*Timeresinsec)/(area*1)
  sRes[1] <-0
  sRes[2:antBox] <- StSt*UHMAD[2:antBox]

  for(i in 3: antBox) {
    sRes[i:antBox] <- sRes[i:antBox] + StSt*UHMAD[i:antBox]
  }

  res <- sum(sRes)

  return(res)
}
