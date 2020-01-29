suppressMessages(suppressWarnings(library(parallel)))

MeanRange <- function(mask,rangeMapFiles,nCores){
  
  mask.vals <- !is.na(values(mask))
  
  cl <- parallel::makeCluster(nCores)
  
  parallel::clusterExport(cl = cl,varlist = c("rangeMapFiles","mask.vals"),
                          envir = environment())
  clusterFunc <- function(i2){
    
    sp <- rangeMapFiles[i2]
    
    r <- raster::raster(sp)
    
    sp.pres <- !is.na(raster::values(r))
    
    sp.pres2 <- rep(NA,length(mask.vals))
    sp.pres2[(mask.vals) & (sp.pres)] <- 1
    
    return(sp.pres2)
    
  }
  
  clusterFunc2 <- function(i2){
    
    sp <- rangeMapFiles[i2]
    
    r <- raster::raster(sp)
    
    sp.pres <- !is.na(raster::values(r))
    
    sp.pres2 <- rep(NA,length(mask.vals))
    sp.pres2[(mask.vals) & (sp.pres)] <- 1
    
    # sp.pres2[(mask.vals) & (sp.pres)] <- sum(sp.pres2,na.rm=TRUE)
    
    
    return(sp.pres2)
    
  }
  
  environment(clusterFunc) <- .GlobalEnv
  environment(clusterFunc2) <- .GlobalEnv
  
  SpPres <- parallel::clusterApply(cl = cl,x = 1:length(rangeMapFiles),
                                   fun = clusterFunc)
  
  SpRange <- parallel::clusterApply(cl = cl,x = 1:length(rangeMapFiles),
                                    fun = clusterFunc2)
  
  stopCluster(cl)
  
  SpPres <- do.call('cbind',SpPres)
  SpRange <- do.call('cbind',SpRange)
  
  MeanRange <- log10(apply(SpRange,1,sum,na.rm=TRUE))/apply(SpPres,1,sum,na.rm=TRUE)
  
  outRaster <- mask
  
  values(outRaster) <- MeanRange
  
  return(outRaster)
  
}