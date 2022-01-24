# author : Julien Ruffault (julien.ruff@gmail.com)
# date : 21/01/2022
# get bias-corrected climate data from 17 climate models (EURO-CORDEX) for the gridcell of the puechabon study site 
# also directly computes data for its use in SurEau-Ecos (right variables and columns names)
# need to be run only one time
# See details about climate data in Pimont et al. (in prep) 
# This script requires to have access to URFM (INRAE) NAS where the climate data are stored 



# notes : get simulations done with François from PEF NAS for the 17 models/ Il faut juste identifier le pixel de PUechabon dans Safran 
#       : include critical thresholds identified in Pimont et al. 2019 in the figure to indicated the increase in the likelihood of going below these critical thresholds in the future 
#       
mainDir = dirname(rstudioapi::getActiveDocumentContext()$path) 

PIX=8766 # Pixel Puechabon dans SAFRAN : 8766
MAIN_CLIMATE_DIR <- ("/Volumes/SHARED_DATA/CLIMAT/CMIP5/Final_Downscaled_France/")  
MODELS_NAME <- c("CNRM_RCA4", "CSIRO_RCA4", "HadGEM_RACMO22", "HadGEM_RCA4", "ICHEC_EC_EARTH_HIRHAM5", "ICHEC_EC_EARTH_RACMO22", "ICHEC_EC_EARTH_RCA4", "IPSL_CM5A_RCA4", "IPSL_CM5A_WRF331F", "MIROC-MIROC5_SMHI-RCA4", "MPI_ESM_RCA4", "MPI_ESM_REMO2009_r1i1p1", "MPI_ESM_REMO2009_r2i1p1", "MPI_RCA4", "SMHI_CCCma_CanESM2_RCA4", "SMHI_NCC_NorESM1_M_RCA4", "SMHI_NOAA_GFDL_GFDL_ESM2M_RCA4")

source("/Volumes/SHARED_DATA/CLIMAT/CMIP5/RHfromSHandT.R")


climat_rcp45 = NULL
climat_rcp85 = NULL
climat_histo = NULL

for (MOD in 1:length(MODELS_NAME)) {# first loop:  MODELS
  
  print(MODELS_NAME[MOD])
  
  refTime_histo = as.Date(read.table(paste0(MAIN_CLIMATE_DIR, "/", MODELS_NAME[MOD], "/time_ref_historical_", MODELS_NAME[MOD], ".csv"),header=F,stringsAsFactors=F)[,1],format='%Y-%m-%d')
  refTime_rcp45 = as.Date(read.table(paste0(MAIN_CLIMATE_DIR, "/", MODELS_NAME[MOD], "/time_ref_rcp4.5_", MODELS_NAME[MOD], ".csv"),header=F,stringsAsFactors=F)[,1],format='%Y-%m-%d')
  refTime_rcp85 = as.Date(read.table(paste0(MAIN_CLIMATE_DIR, "/", MODELS_NAME[MOD], "/time_ref_rcp8.5_", MODELS_NAME[MOD], ".csv"),header=F,stringsAsFactors=F)[,1],format='%Y-%m-%d')
  
  subDir <- paste(MAIN_CLIMATE_DIR, "/", MODELS_NAME[MOD], "/BC_MULTI_by_month", sep = "")
  
  
  data_histo   = readRDS(file=paste(subDir, "/", MODELS_NAME[MOD], "BC_MULTI_MONTH_HISTORICAL_PIX_SAFRAN_N_", PIX, ".rds", sep = ""))
  data_rcp45   = readRDS(file=paste(subDir, "/", MODELS_NAME[MOD], "BC_MULTI_MONTH_RCP45_PIX_SAFRAN_N_", PIX, ".rds", sep = ""))
  data_rcp85   = readRDS(file=paste(subDir, "/", MODELS_NAME[MOD], "BC_MULTI_MONTH_RCP85_PIX_SAFRAN_N_", PIX, ".rds", sep = ""))
  
  
  #histo
  data_histo$RH = RHfromSHandT(data_histo$Tmoy,data_histo$SH*1e-3)
  data_histo$RHmax = RHfromSHandT(data_histo$Tmin,data_histo$SH*1e-3)
  data_histo <- data_histo[,-6]
  head(data_histo)
  colnames(data_histo) <- c('PPT_sum','Tair_mean','Tair_min','Tair_max','WS_mean','RG_sum','RHair_min','RHair_mean','RHair_max')
  data_histo$DATE = as.character(refTime_histo,format= '%d/%m/%Y')
  climat_histo[[MOD]] = data_histo
  
  
  data_rcp45$RH = RHfromSHandT(data_rcp45$Tmoy,data_rcp45$SH*1e-3)
  data_rcp45$RHmax = RHfromSHandT(data_rcp45$Tmin,data_rcp45$SH*1e-3)
  data_rcp45 <- data_rcp45[,-6]
  head(data_rcp45)
  colnames(data_rcp45) <- c('PPT_sum','Tair_mean','Tair_min','Tair_max','WS_mean','RG_sum','RHair_min','RHair_mean','RHair_max')
  data_rcp45$DATE = as.character(refTime_rcp45,format= '%d/%m/%Y')
  climat_rcp45[[MOD]] = data_rcp45  
  
  
  
  data_rcp85$RH = RHfromSHandT(data_rcp85$Tmoy,data_rcp85$SH*1e-3)
  data_rcp85$RHmax = RHfromSHandT(data_rcp85$Tmin,data_rcp85$SH*1e-3)
  data_rcp85 <- data_rcp85[,-6]
  head(data_rcp85)
  colnames(data_rcp85) <- c('PPT_sum','Tair_mean','Tair_min','Tair_max','WS_mean','RG_sum','RHair_min','RHair_mean','RHair_max')
  data_rcp85$DATE = as.character(refTime_rcp85,format= '%d/%m/%Y')
  climat_rcp85[[MOD]] = data_rcp85
  
  
  if (!dir.exists(file.path(mainDir,'climate_projection_data',MODELS_NAME[MOD])))
  {
  dir.create(file.path(mainDir,'climate_projection_data',MODELS_NAME[MOD]))
  }

  write.table(x = data_histo,file = file.path(mainDir,'climate_projection_data',MODELS_NAME[MOD],'climat_histo.csv'),row.names=F,sep=';',dec='.')
  write.table(x = data_rcp45,file = file.path(mainDir,'climate_projection_data',MODELS_NAME[MOD],'climat_rcp45.csv'),row.names=F,sep=';',dec='.')
  write.table(x = data_rcp85,file = file.path(mainDir,'climate_projection_data',MODELS_NAME[MOD],'climat_rcp85.csv'),row.names=F,sep=';',dec='.')
}
  
  
setNames(climat_histo,MODELS_NAME)
setNames(climat_rcp45,MODELS_NAME)
setNames(climat_rcp85,MODELS_NAME)


# save data for future use in SurEau-Ecos
saveRDS(climat_histo,file=file.path(mainDir,'climate_projection_data/climat_histo.csv'))
saveRDS(climat_rcp45,file=file.path(mainDir,'climate_projection_data/climat_rcp45.csv'))
saveRDS(climat_rcp85,file=file.path(mainDir,'climate_projection_data/climat_rcp85.csv'))






