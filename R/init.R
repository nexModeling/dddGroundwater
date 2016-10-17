#' init

#' The funtion initialize the main features of the groundwater:
#' - The maximum capacity of each staturation level,
#' - the Groundwater Storage Capacity (GSC),
#' - The saturation layers
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param UHMAD Unit Hydrograph of the Mean Annual Discharge
#' @param MAD value of the Mean Annual Discharge
#' @param area in squared meters
#' @param modelSaturation list of parameters about the saturation
#'  list(gtcel=,CapacityUpperLevel=b,mLam=c,varLam=d,distr="qgamma")
#' @param modelLayer list of parameters about the Layers
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e), NoL=f)
#' @keywords groundwater
#' @export
#' @examples
#'\dontrun{
#' init()
#' }
 init <-function(Timeresinsec,
                 UHMAD,
                 MAD,
                 area,
                 modelSaturation,
                 modelLayer){

   Res_prob <- grd.Res_prob(NoL=modelLayer$NoL,UHMAD=UHMAD,MAD=MAD,Timeresinsec=Timeresinsec,area=area,modelSaturation=modelSaturation)

   Magkap   <- grd.Magkap(NoL=modelLayer$NoL,Res_prob=Res_prob,CapacityUpperLevel=modelSaturation$CapacityUpperLevel)

   M <- grd.M(NoL=modelLayer$NoL,Res_prob=Res_prob)

   Layers <- matrix(0, ncol=modelLayer$nbStepsDelay[modelLayer$NoL],nrow=modelLayer$NoL)
   for (i in 1:modelLayer$NoL) Layers[i,1:modelLayer$nbStepsDelay[i]] <- 1.0/modelLayer$nbStepsDelay[i]

   res <- list( Magkap = Magkap,
                M      = M,
                Layers = Layers)
  return(res)
}
