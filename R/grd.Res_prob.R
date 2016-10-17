#' grd.Res_prob
#'
#' The function \code{grd.Res_Prob()} computes the groundwater saturation quantiles
#' @param NoL number of saturation level
#' @param UHMAD Unit Hydrograph of the Mean Annual Discharge
#' @param MAD value of the Mean Annual Discharge
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param area in squared meters
#' @param modelSaturation list of parameters about the saturation
#' @return The output is a groundwater saturation quantile for each saturation level
#' @keywords groundwater
#' @export
#' @examples
#' \dontrun{
#' grd.Res_prob()
#' }
grd.Res_prob <-function(NoL,UHMAD,MAD,Timeresinsec,area,modelSaturation) {

  mRes <- saturationSum(MAD=MAD,UHMAD=UHMAD,Timeresinsec=Timeresinsec,area=area)

  Fact <- modelSaturation$mLam/mRes
  stdRes <- (modelSaturation$varLam/Fact^2)^0.5 #se Haan p.51

  GshRes <-mRes^2/stdRes^2
  GscRes <-stdRes^2/mRes

  # probability
  MLev <-seq(1/(NoL-1),1,1/(NoL-1))
  MLev[NoL-1] <- modelSaturation$gtcel

  #calculates the groundwater levels probability associated with quantiles Middel er GshRes*GscRes
  Res_prob <- qgamma(MLev,GshRes,1/GscRes)

  return(Res_prob)
}
