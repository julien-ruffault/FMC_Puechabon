# notes for SA analysis ; reprendre les codes utilises pour le papier de presentation de SurEau-Ecos 
# voir pour les paraemeters a conserver pour l'analayse
#
#Authors : Julien Ruffault (julien.ruff@gmail.com)
#          Nicolas Martin (nicolas.martin@inrae.fr)
# Date   : 24/01/2022
# Script to Launch global sensitivity analyses on FMC at Peuchabon site
# Codes and procedures are derived from the sensitivity analysis in Ruffault et al. submitted,GMD
# Selected parameters : 'P50_VC_Leaf'
#                       'gmin20'
#                       'epsilon_Sym"
#                       "PiFullTurgor"
#                       "Q10_1_gmin"
#                       "TPhase_gmin"


  

# clean environment
rm(list=ls(all=TRUE));gc()

# define working directory as the directory of Sureau
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) 
#define directory where inputs parameters are stocked 
directoryToRefSimu = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/sensitivity_FMC/"
mainDirOutpout =directoryToRefSimu


library(sensobol)
library(foreach)
library(doParallel)
library(lubridate)


# climate and soils won't change throughout the sensitivity analysis and are loaded here 
climateData_path  <- paste0(mainDir,'/Input_parameters/Climat_Puechabon_site.csv') # <-- indicate here the path to input climate data 
modeling_options  <- create.modeling.options(timeStepForEvapo=2,
                                             constantClimate=F,
                                             defoliation = T,
                                             compOptionsForEvapo = c("Fast"),
                                             thresholdMortality=99)
soilFile <- read.soil.file(paste0(mainDir,'/Input_parameters/Soil_Puechabon_OK_LFMC_VG.csv'), modeling_options=modeling_options)


# Define general parameters for the simulation
params=c('P50_VC_Leaf',
         'gmin20',
         "epsilon_Sym",
         "PiFullTurgor",
         "Q10_1_gmin",
         "TPhase_gmin",
         "apoFrac_Leaf",
         "P50_gs")


percentV = 20/100
N <- 100 # number for initial sampling / about 30000-100000 for full models
k <- length(params) # number of parameters


R <- 10^3
type <- "norm"
conf <- 0.95


# for parallelistation 
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload the computer
registerDoParallel(cl)

#SWCmean <- 147


vegFile <- read.vegetation.file(filePath =  paste0(mainDir,'/Input_parameters/vegetation_Puechabon.csv'), modeling_options = modeling_options)


PARAMS <- sobol_matrices(params = params, N = N, order = "first")
print(paste0('number of simulations : ',nrow(PARAMS)))


#PARAMS[, "LAImax"]       <- qunif(PARAMS[, "LAImax"], 2.2 - percentV * 2.2, 2.2 + percentV * 2.2)
#PARAMS[, "SWC"]          <- qunif(PARAMS[, "SWC"], SWCmean - SWCmean * percentV, SWCmean + SWCmean * percentV)
PARAMS[, "P50_VC_Leaf"]  <- qunif(PARAMS[, "P50_VC_Leaf"] , vegFile$P50_VC_Leaf + vegFile$P50_VC_Leaf * percentV, vegFile$P50_VC_Leaf - vegFile$P50_VC_Leaf * percentV)
PARAMS[, "gmin20"]       <- qunif(PARAMS[, "gmin20"]      , vegFile$gmin20 - percentV * vegFile$gmin20, vegFile$gmin20 + percentV * vegFile$gmin20)
PARAMS[, "P50_gs"]       <- qunif(PARAMS[, "P50_gs"]      , vegFile$P50_gs + vegFile$P50_gs * percentV, vegFile$P50_gs - vegFile$P50_gs * percentV)
PARAMS[, "epsilon_Sym"]  <- qunif(PARAMS[, "epsilon_Sym"] , vegFile$epsilonSym_Leaf - percentV * vegFile$epsilonSym_Leaf, vegFile$epsilonSym_Leaf + percentV * vegFile$epsilonSym_Leaf)
PARAMS[, "PiFullTurgor"] <- qunif(PARAMS[, "PiFullTurgor"], vegFile$PiFullTurgor_Leaf + percentV * vegFile$PiFullTurgor_Leaf, vegFile$PiFullTurgor_Leaf - percentV * vegFile$PiFullTurgor_Leaf)
PARAMS[, "apoFrac_Leaf"] <- qunif(PARAMS[, "apoFrac_Leaf"], vegFile$apoFrac_Leaf - percentV * vegFile$apoFrac_Leaf, vegFile$apoFrac_Leaf + percentV * vegFile$apoFrac_Leaf)
PARAMS[, "Q10_1_gmin"]   <- qunif(PARAMS[, "Q10_1_gmin"]  , vegFile$Q10_1_gmin - percentV * vegFile$Q10_1_gmin, vegFile$Q10_1_gmin + percentV * vegFile$Q10_1_gmin)
#PARAMS[, "Q10_2_gmin"]   <- qunif(PARAMS[, "Q10_2_gmin"], vegFile$Q10_2_gmin - percentV * vegFile$Q10_2_gmin, vegFile$Q10_2_gmin + percentV * vegFile$Q10_2_gmin)
PARAMS[, "TPhase_gmin"]   <- qunif(PARAMS[, "TPhase_gmin"], vegFile$TPhase_gmin - percentV * vegFile$TPhase_gmin, vegFile$TPhase_gmin + percentV * vegFile$TPhase_gmin)
#PARAMS[, "LDMC"]          <- qunif(PARAMS[, "LDMC"], vegFile$LDMC - percentV * vegFile$LDMC, vegFile$LDMC + percentV * vegFile$LDMC)
#PARAMS[, "LMA"]          <- qunif(PARAMS[, "LMA"], vegFile$LMA - percentV * vegFile$LMA, vegFile$LMA + percentV * vegFile$LMA)
#   

#DEPTH3 =PARAMS[,"SWC"]  = ((soilFile$saturation_capacity_vg-soilFile$residual_capacity_vg)*1000)


#DD1 = ((soilFile$saturation_capacity_vg-soilFile$residual_capacity_vg)*1000)*(1-(soilFile$RFC_1)/100)*soilFile$depth1
#DD2 = ((soilFile$saturation_capacity_vg-soilFile$residual_capacity_vg)*1000)*(1-(soilFile$RFC_2)/100)*(soilFile$depth2-soilFile$depth1)
#DEPTH3 =soilFile$depth2 + (PARAMS[,"SWC"]-(DD1+DD2))/(((soilFile$saturation_capacity_vg-soilFile$residual_capacity_vg)*1000)*(1-(soilFile$RFC_3)/100))



# write output directory 
Out_dir <- file.path(mainDirOutpout)
dir.create(Out_dir,showWarnings=F,recursive=T)
write.csv(PARAMS,paste0(Out_dir,'/PARAMS_.csv'),row.names=F)


foreach(i=1:nrow(PARAMS),.packages=c('lubridate','insol')) %dopar% {
  print(i)
  output_path = paste0(Out_dir,'/SA_FMC_',i,'.csv')
  # 
  simulation_parameters <- create.simulation.parameters(startYearSimulation = 2017,
                                                        endYearSimulation = 2017,
                                                        mainDir= mainDir,
                                                        resolutionOutput = "yearly",
                                                        outputType = 'yearly_forFMC',
                                                        overWrite = T,
                                                        outputPath = output_path)
  
  
  # simulation_parameters <- create.simulation.parameters(startYearSimulation = 2016,
  #                                                       endYearSimulation = 2016,
  #                                                       mainDir= mainDir,
  #                                                       resolutionOutput = "subdaily",
  #                                                       outputType = 'diagnostic_subdaily',
  #                                                       overWrite = T,
  #                                                       outputPath = output_path)
  
  climate_data     <- create.climate.data(filePath = climateData_path, modeling_options = modeling_options, simulation_parameters = simulation_parameters) #
  
  
  #climate_data$WS_mean <- 1

  stand_parameters <- create.stand.parameters(LAImax=2.2,lat = 48.73, lon = 6.23)
  #stand_parameters <- create.stand.parameters(LAImax=PARAMS[,"LAImax"][i],lat = 48.73, lon = 6.23)
  #stand_parameters <- create.stand.parameters(LAImax=6,lat = 48.73, lon = 6.23)
  
  vegFile$gmin20=PARAMS[,"gmin20"][i]
  vegFile$gmin_T=PARAMS[,"gmin20"][i]
  
  vegFile$P50_gs = PARAMS[,"P50_gs"][i]
  
  vegFile$P50_VC_Leaf=PARAMS[,"P50_VC_Leaf"][i]
  vegFile$P50_VC_Stem = PARAMS[,"P50_VC_Leaf"][i]
  
  vegFile$apoFrac_Leaf = PARAMS[,"apoFrac_Leaf"][i]
  vegFile$epsilonSym_Leaf = PARAMS[,"epsilon_Sym"][i]
  vegFile$epsilonSym_Stem = PARAMS[,"epsilon_Sym"][i]
  vegFile$PiFullTurgor_Leaf = PARAMS[,"PiFullTurgor"][i]
  vegFile$PiFullTurgor_Trunk = PARAMS[,"PiFullTurgor"][i]
  vegFile$Q10_1_gmin = PARAMS[,"Q10_1_gmin"][i]
  #vegFile$Q10_2_gmin = PARAMS[,"Q10_2_gmin"][i]
  vegFile$TPhase_gmin  = PARAMS[,"TPhase_gmin"][i]
  
  #vegFile$LDMC  = PARAMS[,"LDMC"][i]
  #vegFile$LMA = PARAMS[,"LMA"][i]
  
  #soilFile$depth3 = DEPTH3[i]
  
  soil_parameters  <- create.soil.parameters(listOfParameters = soilFile, modeling_options=modeling_options)
  vegetation_parameters <- create.vegetation.parameters(listOfParameters= vegFile, stand_parameters = stand_parameters, soil_parameter = soil_parameters,modeling_options = modeling_options)
  

  run.SurEau_Ecos(modeling_options = modeling_options ,
                  simulation_parameters = simulation_parameters, 
                  climate_data = climate_data,
                  stand_parameters = stand_parameters, 
                  soil_parameters = soil_parameters,
                  vegetation_parameters = vegetation_parameters)
}



# # Output loading an plotting  ------------------------------------------
# filename  =  paste0(Out_dir,'/SA_FMC_',i,'.csv')
# DATA      = read.csv(filename,header=T, dec='.', sep="")
# DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
# plot(DATA$Psi_LApo)
# 

# directoryToRefSimu = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/sensitivity_FMC_2018/"
# mainDirOutpout =directoryToRefSimu
# Out_dir <- file.path(mainDirOutpout)
# 
# # quick analyses to check results 
# Y1 = NULL
# Y2 = NULL
# Y3 = NULL
# for (i in 1:nrow(PARAMS))
# {
#   io =read.csv(paste0(Out_dir,'/SA_FMC_',i,'.csv'),header=T, dec='.',sep="")
#   Y1[i]  = io$yearly_FMCCanopy_min
#   Y2[i]  = io$yearly_nbDayLFMC_67
#   Y3[i]  = io$yearly_LFMC_min
# }
# 
# 



directoryToRefSimu = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/sensitivity_FMC/"
mainDirOutpout =directoryToRefSimu
Out_dir <- file.path(mainDirOutpout)

# quick analyses to check results 
Z1 = NULL
Z2 = NULL
Z3 = NULL
for (i in 1:nrow(PARAMS))
{
  io =read.csv(paste0(Out_dir,'/SA_FMC_',i,'.csv'),header=T, dec='.',sep="")
  Z1[i]  = io$yearly_FMCCanopy_min
  Z2[i]  = io$yearly_nbDayLFMC_67
  Z3[i]  = io$yearly_LFMC_min
}

A <- sobol_indices(Y = Z3, N = N, params = params, order="first",boot = F)
plot(A)
A <- sobol_indices(Y = Z1, N = N, params = params, order="first",boot = F)
plot(A)
