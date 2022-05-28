# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Launcher Puechabon_LFMC avec Van-Genuchten formulation 
# Authors : Nicolas Martin (nicolas.martin@inrae.fr)
#           Julien Ruffault (julien.ruff@gmail.com)
# Date : 17/01/2022           
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
climateData_path          <- paste0(mainDir,'/Input_parameters/Climat_Puechabon_site.csv')
soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil_Puechabon_OK_LFMC_VG.csv')
#soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil_Puechabon_OK_LFMC_VG2.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/vegetation_Puechabon.csv')

# load SurEau-Ecos ------------------------------------------------------------
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) 
modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                             transpirationModel = 'Jarvis',
                                             defoliation = T,
                                             stomatalRegFormulation = "Sigmoid",
                                             PedoTransferFormulation="VG") 

stand_parameters      <- create.stand.parameters(LAImax = 2.2, lat = 43.75, lon = 3.6)
soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, modeling_options = modeling_options, offSetPsoil = .3)

vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)


#Measured parameters
#---------------------------------------------------------------------------------------
output_path_measured <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_Measured.csv')
simulation_parameters <- create.simulation.parameters(startYearSimulation = 2016,                       
                                                      endYearSimulation = 2018,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path_measured)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #

vegetation_parameters$vol_Stem <- 10
vegetation_parameters$LDMC = 579
vegetation_parameters$PiFullTurgor_Leaf = -2.81
vegetation_parameters$epsilonSym_Leaf = 12.75
vegetation_parameters$apoFrac_Leaf = (1-0.56)
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)



#------
#Adjusted all years
output_path_adjAll <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_AdjustedAllYears.csv')
simulation_parameters <- create.simulation.parameters(startYearSimulation = 2016,                       
                                                      endYearSimulation = 2018,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path_adjAll)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #

vegetation_parameters$vol_Stem <- 10
vegetation_parameters$LDMC = 567
vegetation_parameters$PiFullTurgor_Leaf = -2.67
vegetation_parameters$epsilonSym_Leaf = 15
vegetation_parameters$apoFrac_Leaf = (1-0.56)
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

#-----------------------------------
#Adjusted per year
#------
#2016
output_path_adj_2016 <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_Adjusted_2016.csv')
simulation_parameters <- create.simulation.parameters(startYearSimulation = 2016,                       
                                                      endYearSimulation = 2016,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path_adj_2016)
climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #

vegetation_parameters$vol_Stem <- 10
vegetation_parameters$LDMC = 573
vegetation_parameters$PiFullTurgor_Leaf = -2.45
vegetation_parameters$epsilonSym_Leaf = 15
vegetation_parameters$apoFrac_Leaf = (1-0.56)
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

#-------------
#2017
output_path_adj_2017 <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_Adjusted_2017.csv')
simulation_parameters <- create.simulation.parameters(startYearSimulation = 2017,                       
                                                      endYearSimulation = 2017,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path_adj_2017)
climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #

vegetation_parameters$vol_Stem <- 10
vegetation_parameters$LDMC = 554
vegetation_parameters$PiFullTurgor_Leaf = -2.94
vegetation_parameters$epsilonSym_Leaf = 7
vegetation_parameters$apoFrac_Leaf = (1-0.56)
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)


#-------------
#2018
output_path_adj_2018 <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_Adjusted_2018.csv')
simulation_parameters <- create.simulation.parameters(startYearSimulation = 2018,                       
                                                      endYearSimulation = 2018,
                                                      mainDir = mainDir,
                                                      outputType = 'diagnostic_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path_adj_2018)
climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #

vegetation_parameters$vol_Stem <- 10
vegetation_parameters$LDMC = 566
vegetation_parameters$PiFullTurgor_Leaf = -3
vegetation_parameters$epsilonSym_Leaf = 15
vegetation_parameters$apoFrac_Leaf = (1-0.56)
run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)



#END