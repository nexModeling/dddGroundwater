#' Groundwater initialization

#' The funtion initializes the main features of the groundwater:
#' - The maximum capacity of each staturation level,
#' - the Groundwater Storage Capacity (GSC),
#' - The saturation layers
#' @param method method for the initialization, "load", "source", "manual"
#' @param Magkap Magkap
#' @param M  Groundwater Storage Capacity (GSC)
#' @param Layers saturation layers
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param UHMAD Unit Hydrograph of the Mean Annual Discharge
#' @param MAD value of the Mean Annual Discharge
#' @param area in squared meters
#' @param modelSaturation list of parameters about the saturation
#'  list(gtcel,CapacityUpperLevel,mLam,varLam,distr)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param,NoL)
#' @keywords groundwater
#' @export
#' @examples
#'\dontrun{
#' init.groundwater)
#' }
init.groundwater <-function(method=NULL,Magkap=NULL,M=NULL,Layers=NULL,Timeresinsec=NULL,UHMAD=NULL,MAD=NULL,area=NULL,modelSaturation=NULL,modelLayer=NULL){

  groundwater <- switch(method,
    "manual"    = init.manual(Magkap=Magkap,M=M,Layers=Layers),
    "processed" = init.processed(Timeresinsec=Timeresinsec,UHMAD=UHMAD,MAD=MAD,area=modelArea$totarea,modelSaturation=modelSaturation,modelLayer=modelLayer),
    "load"      = init.load(path=path),
    "source"    = init.source(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(groundwater)
}


init.manual <- function(Magkap,M,Layers){
  res <- list(Magkap = Magkap,
               M      = M,
               Layers = Layers)
  return(res)
}


init.load <- function(path){
  load(paste0(path,"groundwater.rda"))
  return(groundwater)
}


init.source <- function(path){
  source(paste0(path,"groundwater.R"),local=TRUE)
  return(groundwater)
}


init.processed <- function(Timeresinsec,UHMAD,MAD,area,modelSaturation,modelLayer){

  if ( (!is.null(Timeresinsec)) && (!is.null(UHMAD)) && (!is.null(NAD)) && (!is.null(area)) &&
       (!is.null(modelSaturation)) && (!is.null(modelLayer))   ) {
    Res_prob <- grd.Res_prob(NoL=modelLayer$NoL,UHMAD=UHMAD,MAD=MAD,Timeresinsec=Timeresinsec,area=area,modelSaturation=modelSaturation)

    Magkap   <- grd.Magkap(NoL=modelLayer$NoL,Res_prob=Res_prob,CapacityUpperLevel=modelSaturation$CapacityUpperLevel)

    M <- grd.M(NoL=modelLayer$NoL,Res_prob=Res_prob)

    Layers <- matrix(0, ncol=modelLayer$nbStepsDelay[modelLayer$NoL],nrow=modelLayer$NoL)
    for (i in 1:modelLayer$NoL) Layers[i,1:modelLayer$nbStepsDelay[i]] <- 1.0/modelLayer$nbStepsDelay[i]

    res <- list( Magkap = Magkap,
                 M      = M,
                 Layers = Layers)
   return(res)
  } else stop("NULL arguments in init.processed Soil Discharge")


}