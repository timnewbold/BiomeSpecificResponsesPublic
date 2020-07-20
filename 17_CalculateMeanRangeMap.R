outDir <- "17_CalculateMeanRangeMap/"

dataDir <- "0_data/"
rangesDir <- "0_ranges/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(RColorBrewer)))

print(sessionInfo())

source("MeanRange.R")

nCores <- parallel::detectCores()-1

rangeMapFiles <- paste(rangesDir,dir(path = rangesDir,recursive = TRUE),sep="")
rangeMapFiles <- rangeMapFiles[!grepl("aux",rangeMapFiles)]
rangeMapFiles <- rangeMapFiles[!grepl("log.txt",rangeMapFiles)]

mask <-  raster(paste(dataDir,"Mask.grd",sep=""))

templateRas <- raster(rangeMapFiles[1])

mask <- raster::resample(mask,templateRas,method='ngb')

meanrange <- MeanRange(mask = mask,rangeMapFiles = rangeMapFiles,nCores = nCores)
values(meanrange)[values(meanrange) == -Inf] <- NA

saveRDS(object = meanrange,file = paste0(outDir,"MeanRangeSizeMap.rds"))

png(filename = paste0(outDir,"MeanRangeSize.png"),width = 17.5,height = 8.75,units = "cm",res = 1200)

par(mar=c(0,0,0,0))

brks <- quantile(x = values(meanrange),probs=seq(from=0,to=1,length.out=11),na.rm=TRUE)
brks[1] <- 0
brks[11] <- brks[11]+0.01

plot(meanrange,breaks=brks,col=brewer.pal(n = 10,name = "RdYlBu"))

invisible(dev.off())

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()