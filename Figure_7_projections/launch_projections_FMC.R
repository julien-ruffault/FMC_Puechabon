# author : Julien Ruffault (julien.ruff@gmail.com)
# date = 22/01/2022
# launcher to run FMC estimations on 
# Need to add a new output yearly format for LFMC in SurEau-Ecos (no need to keep track of all subdaily or daily inputs) 


rm(list=ls())
gc()




# Set paths that will not change 
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) 
soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil_Puechabon_OK_LFMC_VG.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/vegetation_Puechabon.csv')


MODELS_NAME <- c("CNRM_RCA4", "CSIRO_RCA4", "HadGEM_RACMO22", "HadGEM_RCA4", "ICHEC_EC_EARTH_HIRHAM5", "ICHEC_EC_EARTH_RACMO22", "ICHEC_EC_EARTH_RCA4", "IPSL_CM5A_RCA4", "IPSL_CM5A_WRF331F", "MIROC-MIROC5_SMHI-RCA4", "MPI_ESM_RCA4", "MPI_ESM_REMO2009_r1i1p1", "MPI_ESM_REMO2009_r2i1p1", "MPI_RCA4", "SMHI_CCCma_CanESM2_RCA4", "SMHI_NCC_NorESM1_M_RCA4", "SMHI_NOAA_GFDL_GFDL_ESM2M_RCA4")


#for (MOD in 1:length(MODELS_NAME)) 
#{
 MOD=6
climateData_path          <-  paste0(mainDir,'/Figure_7_projections/climate_projection_data/',MODELS_NAME[MOD],'/climat_rcp85.csv')
output_path               <-  paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_rcp85.csv')

# load SurEau-Ecos ------------------------------------------------------------

modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                             transpirationModel = 'Jarvis',
                                             defoliation = T,
                                             stomatalRegFormulation = "Sigmoid",
                                             PedoTransferFormulation="VG") 

simulation_parameters <- create.simulation.parameters(startYearSimulation = 2090,                       
                                                      endYearSimulation = 2098,
                                                      mainDir = mainDir,
                                                      resolutionOutput = "yearly",
                                                      outputType = 'yearly_forFMC',
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




