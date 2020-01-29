import platform
import socket

# Define the locations of programs
if 'WCMC-LT-01636' == socket.gethostname():
    # Tim's Windows PC
    arcpy = 'c:/python26/ArcGIS10.0/python.exe'
    python = 'c:/Python34/python.exe'
    R = 'c:/Program Files/R/R-3.1.2/bin/x64/Rscript.exe'
elif 'WCMC-PC-01686' == socket.gethostname():
    arcpy = 'c:/python26/ArcGIS10.0/python.exe'
    python = 'c:/Python34/python.exe'
    R = 'c:/Program Files/R/R-3.1.2/bin/x64/Rscript.exe'
elif 'ucbttne-PC' == socket.gethostname():
    # Tim's UCL PC
    arcpy = 'c:/python27/ArcGIS10.3/python.exe'
    python = 'c:/Python35/python.exe'
    R = 'c:/Program Files/R/R-3.3.2/bin/x64/Rscript.exe'
elif 'ucbttne-LT' == socket.gethostname():
    # Tim's UCL laptop
    arcpy = 'c:/python27/ArcGIS10.3/python.exe'
    python = 'c:/Python35/python.exe'
    R = 'c:/Program Files/R/R-3.2.2/bin/x64/Rscript.exe'
elif 'ucbttne-PC2' == socket.gethostname():
    arcpy = 'c:/python27/ArcGIS10.4/python.exe'
    python = 'c:/Python36/python.exe'
    R = 'c:/Program Files/R/R-3.3.2/bin/x64/Rscript.exe'
elif 'ucbttne-LT2'==socket.gethostname():
    arcpy = 'c:/python27/ArcGIS10.4/python.exe'
    python = 'c:/Python36/python.exe'
    R = 'c:/Program Files/R/R-3.3.2/bin/x64/Rscript.exe'

STAGES = [ ('1', R,              '1_PrepareDiversityData.R'),
           ('2', R,              '2_PrepareSiteData.R'),
           ('4', R,              '4_RunModels.R'),
           ('5', R,              '5_RunFinalModels.R'),
           ('6', R,              '6_PlotLandUseModels.R'),
           ('7', R,              '7_CalculateClimateDeltas.R'),
           ('8', R,              '8_EstimateClimateSensitivity.R'),
           ('9', R,              '9_ProcessBiomeMap.R'),
           ('10', R,             '10_EstimateClimateSensitivityByBiome.R'),
           ('11', R,             '11_PlotClimateLandUseSensitivityRelationship.R'),
           ('14', R,             '14_PlotBiomeSensitivityMaps.R'),
           ('15', R,             '15_RunExplanatoryModels.R'),
           ('16', R,             '16_PlotExplanatoryModels.R'),
           ('17', R,             '17_CalculateMeanRangeMap.R'),
           ('18', R,             '18_RunExplanatoryModelsClimate.R'),
           ('19', R,             '19_CompareScenarioClimateSensitivities.R'),
         ]
