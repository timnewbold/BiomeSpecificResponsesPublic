outDir <- "1_PrepareDiversityData/"

dataDir <- "0_data/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(predictsFunctions)))

print(sessionInfo())

cat('Loading database extracts\n')
diversity<-readRDS(paste(dataDir,"database.rds",sep=""))

cat('Selecting appropriate data\n')
diversity <- diversity[apply(diversity[,c('Longitude','Latitude')],1,
                             function(r) all(!is.na(r))),]

# Drop studies that focused on a single species
diversity <- diversity[!diversity$Rank_of_study_common_taxon %in% 
                         c('Infraspecies','Species'),] 

cat('Correcting for sampling effort\n')
diversity <- CorrectSamplingEffort(diversity)

cat('Merging sites\n')
diversity <- MergeSites(diversity,silent = TRUE)

cat('Saving diversity data\n')
save(diversity,file=paste(outDir,"diversity_data.Rd",sep=""))

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()