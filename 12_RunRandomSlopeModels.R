suppressMessages(suppressWarnings(library(StatisticalModels)))

dataDir <- "2_PrepareSiteData/"

outDir <- "12_RunRandomSlopeModels/"

load(paste0(dataDir,"modelling_data.Rd"))

sites.div <- droplevels(sites.div)

randomSlopeModelRich <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                              fitFamily = "poisson",fixedStruct = "LandUse3",
                              randomStruct = "(1+LandUse3|SS)+(1|SSBS)",REML = TRUE)

saveRDS(object = randomSlopeModelRich,file = paste0(outDir,"RichnessRandomSlopeModel.rds"))
