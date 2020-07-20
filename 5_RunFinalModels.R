outDir <- "5_RunFinalModels/"

dataDir <- "2_PrepareSiteData/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(StatisticalModels)))

print(sessionInfo())

cat('Loading data\n')

load(paste0(dataDir,"modelling_data.Rd"))

sites.div$LogAbund <- log(sites.div$Total_abundance+1)

sites.div <- droplevels(sites.div)

cat('Running richness model\n')
finalModelRich <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                        fitFamily = "poisson",fixedStruct = "LandUse3*Biome2",
                        randomStruct = "(1|SS)+(1|SSBS)",REML = TRUE)

cat('Running abundance model\n')
finalModelAbund <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                         fitFamily = "gaussian",fixedStruct = "LandUse3*Biome2",
                         randomStruct = "(1|SS)",REML = TRUE)

cat('Running endemicity model\n')
finalModelEndem <- GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                         fitFamily = "gaussian",fixedStruct = "LandUse3*Biome2",
                         randomStruct = "(1|SS)",REML = TRUE)

save(finalModelRich,finalModelAbund,finalModelEndem,
     file = paste0(outDir,"FinalModels.Rd"))

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()


