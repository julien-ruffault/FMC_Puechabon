# author : Julien Ruffault (julien.ruff@gmail.com)
# date = 22/01/2022
# launcher to run FMC estimations on 
# Need to add a new output yearly format for LFMC in SurEau-Ecos (no need to keep track of all subdaily or daily inputs) 


rm(list=ls())
gc()
library(sensobol)
library(foreach)
library(doParallel)
library(lubridate)
 
# Set paths that will not change 
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) 
soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil_Puechabon_OK_LFMC_VG.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/vegetation_Puechabon.csv')


MODELS_NAME <- c("CNRM_RCA4", "CSIRO_RCA4", "HadGEM_RACMO22", "HadGEM_RCA4", "ICHEC_EC_EARTH_HIRHAM5", "ICHEC_EC_EARTH_RACMO22", "ICHEC_EC_EARTH_RCA4", "IPSL_CM5A_RCA4", "IPSL_CM5A_WRF331F", "MIROC-MIROC5_SMHI-RCA4", "MPI_ESM_RCA4", "MPI_ESM_REMO2009_r1i1p1", "MPI_ESM_REMO2009_r2i1p1", "MPI_RCA4", "SMHI_CCCma_CanESM2_RCA4", "SMHI_NCC_NorESM1_M_RCA4", "SMHI_NOAA_GFDL_GFDL_ESM2M_RCA4")


# model selection based on Pimont et al. 
MODELS_NAME <-  c("CSIRO_RCA4",
                   "HadGEM_RACMO22", 
                   "HadGEM_RCA4",
                   "ICHEC_EC_EARTH_RACMO22",
                   "ICHEC_EC_EARTH_RCA4",
                   "IPSL_CM5A_RCA4",
                   "MIROC-MIROC5_SMHI-RCA4", 
                   "MPI_ESM_RCA4", 
                   "MPI_ESM_REMO2009_r1i1p1", 
                   "MPI_ESM_REMO2009_r2i1p1", 
                   "SMHI_CCCma_CanESM2_RCA4", 
                   "SMHI_NCC_NorESM1_M_RCA4",
                   "SMHI_NOAA_GFDL_GFDL_ESM2M_RCA4")


cores=detectCores()
cl <- makeCluster(cores[1]-3) #not to overload the computer
registerDoParallel(cl)


# Rcp85

foreach(MOD=(1:length(MODELS_NAME)),.packages=c('lubridate','insol')) %dopar% {

  
  if (!dir.exists(file.path(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD])))
  {
    dir.create(file.path(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD]))
  }
  
  
climateData_path          <-  paste0(mainDir,'/Figure_7_projections/climate_projection_data/',MODELS_NAME[MOD],'/climat_rcp85.csv')
output_path               <-  paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_rcp85.csv')


modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                             transpirationModel = 'Jarvis',
                                             timeStepForEvapo=2,
                                             defoliation = T,
                                             stomatalRegFormulation = "Sigmoid",
                                             PedoTransferFormulation="VG") 

simulation_parameters <- create.simulation.parameters(startYearSimulation = 2006,                       
                                                      endYearSimulation = 2100,
                                                      mainDir = mainDir,
                                                      resolutionOutput = "yearly",
                                                      outputType = 'yearly_forFMC',
                                                      # resolutionOutput = "subdaily",
                                                      # outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

stand_parameters      <- create.stand.parameters(LAImax = 2.2, lat = 43.75, lon = 3.6)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, modeling_options = modeling_options, offSetPsoil = .3)

vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

}


# Historical period 
foreach(MOD=(1:length(MODELS_NAME)),.packages=c('lubridate','insol')) %dopar% {
  
  if (!dir.exists(file.path(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD])))
  {
    dir.create(file.path(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD]))
  }
  
  climateData_path          <-  paste0(mainDir,'/Figure_7_projections/climate_projection_data/',MODELS_NAME[MOD],'/climat_histo.csv')
  output_path               <-  paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_histo.csv')
  
  
  modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                               transpirationModel = 'Jarvis',
                                               timeStepForEvapo=2,
                                               defoliation = T,
                                               stomatalRegFormulation = "Sigmoid",
                                               PedoTransferFormulation="VG") 
  
  simulation_parameters <- create.simulation.parameters(startYearSimulation = 1985,                       
                                                        endYearSimulation = 2005,
                                                        mainDir = mainDir,
                                                        resolutionOutput = "yearly",
                                                        outputType = 'yearly_forFMC',
                                                        # resolutionOutput = "subdaily",
                                                        # outputType = 'diagnostic_subdaily',
                                                        overWrite = T,
                                                        outputPath = output_path)
  
  stand_parameters      <- create.stand.parameters(LAImax = 2.2, lat = 43.75, lon = 3.6)
  
  climate_data          <- create.climate.data(filePath = climateData_path, 
                                               modeling_options = modeling_options,
                                               simulation_parameters = simulation_parameters) #
  soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, modeling_options = modeling_options, offSetPsoil = .3)
  
  vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                        stand_parameters = stand_parameters, 
                                                        soil_parameter = soil_parameters,
                                                        modeling_options = modeling_options)
  
  run.SurEau_Ecos(modeling_options = modeling_options ,
                  simulation_parameters = simulation_parameters, 
                  climate_data = climate_data,
                  stand_parameters = stand_parameters, 
                  soil_parameters = soil_parameters,
                  vegetation_parameters = vegetation_parameters)
  
}

# 
# filename = paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_rcp85.csv')
# DATA      = read.csv(filename,header=T, dec='.', sep="")
# head(DATA)
# 
# plot(DATA$Time,DATA$yearly_FMCCanopy_min,type='l',col=2)
# lines(DATA$Time,DATA$yearly_LFMC_min,type='l',col=1)
# 
# # DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
# plot(DATA$LAI)
# plot(DATA$LAIdead)
# plot(DATA$FMCCanopy)
# lines(DATA$LFMC,col=2)
# plot(DATA$PLC_Leaf)
# plot(DATA$Psi_LSym,type='l')
