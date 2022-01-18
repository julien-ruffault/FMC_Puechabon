# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Launcher Puechabon_LFMC avec campbell formulation 
# Authors : Nicolas Martin (nicolas.martin@inrae.fr)
#           Julien Ruffault (julien.ruff@gmail.com)
# Date : 17/01/2022           
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
climateData_path          <- paste0(mainDir,'/Input_parameters/Climat_Puechabon_site.csv')
soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil_Puechabon_OK_LFMC_Campbell.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/vegetation_Puechabon.csv')
output_path               <-  paste0(mainDir,'/test_FMC_Puechabon.csv')

# load SurEau-Ecos ------------------------------------------------------------
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) 

# Create input list files and define options -----------------------------------
modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                             transpirationModel = 'Jarvis',
                                             defoliation = T,
                                             stomatalRegFormulation = "Sigmoid",
                                             PedoTransferFormulation="Campbell") #PiecewiseLinear

simulation_parameters <- create.simulation.parameters(startYearSimulation = 2016,                       
                                                      endYearSimulation = 2018,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

stand_parameters      <- create.stand.parameters(LAImax = 2.2, lat = 43.75, lon = 3.6)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters)

soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, modeling_options = modeling_options, offSetPsoil = .3)

#modif
vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)

PlotTheStandAndPlant(vegetation_parameters, soil_parameters, modeling_options, openWindow=T)

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

