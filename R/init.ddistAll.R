#' ddistAll initialization

#' The funtion initializes the main features of the groundwater:
#' - The maximum capacity of each staturation level,
#' - the Groundwater Storage Capacity (GSC),
#' - The saturation layers
#' @param method method for the initialization, "load", "manual"
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
    "manual"    = init.ddistAll.manual(S=S,ddistx=ddistx,ddist=ddist),
    "load"      = init.ddistAll.load(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(ddistAll)
}


init.ddistAll.manual <- function(S,ddistx,ddist){
  res <- list(S      = S,
              ddistx = ddistx,
              ddist  = ddist )
  return(res)
}


init.ddistAll.load <- function(path){
  env <- environment()
  path <- normalizePath(file.path(path,"ddistAll.rda"),mustWork = FALSE)
  load(path, envir=env)
  ddistAll <- get("ddistAll",envir = env)
  return(ddistAll)
}
