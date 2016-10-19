#' ddistAll initialization

#' The funtion initializes the main features of the groundwater:
#' - The maximum capacity of each staturation level,
#' - the Groundwater Storage Capacity (GSC),
#' - The saturation layers
#' @param method method for the initialization, "load", "source", "manual"
#' @param path directory where to get the files
#' @param S deficit
#' @param ddistx current capacity of each level (in mm)
#' @param ddist states of each saturation level
#' @keywords groundwater
#' @export
#' @examples
#'\dontrun{
#' init.ddistAll())
#' }

init.ddistAll <-function(method=NULL,path=NULL,S=NULL,ddistx=NULL,ddist=NULL){

  ddistAll <- switch(method,
    "manual"    = init.manual(S=S,ddistx=ddistx,ddist=ddist),
    "load"      = init.load(path=path),
    "source"    = init.source(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(ddistAll)
}


init.manual <- function(S,ddistx,ddist){
  res <- list(S      = S,
              ddistx = ddistx,
              ddist  = ddist )
  return(res)
}


init.load <- function(path){
  load(paste0(path,"ddistAll.rda"))
  return(ddistAll)
}


init.source <- function(path){
  source(paste0(path,"ddistAll.R"),local=TRUE)
  return(ddistAll)
}
