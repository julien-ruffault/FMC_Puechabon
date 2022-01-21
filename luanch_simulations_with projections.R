# notes : get simulations done with François from PEF NAS for the 17 models/ Il faut juste identifier le pixel de PUechabon dans Safran 
#       : include critical thresholds identified in Pimont et al. 2019 in the figure to indicated the increase in the likelihood of going below these critical thresholds in the future 
#       


# Pixel Puechabon dans SAFRAN : 8766
 PIX=8766

MAIN_CLIMATE_DIR <- ("/Volumes/SHARED_DATA/CLIMAT/CMIP5/Final_Downscaled_France/")  
MODELS_NAME <- c("CNRM_RCA4", "CSIRO_RCA4", "HadGEM_RACMO22", "HadGEM_RCA4", "ICHEC_EC_EARTH_HIRHAM5", "ICHEC_EC_EARTH_RACMO22", "ICHEC_EC_EARTH_RCA4", "IPSL_CM5A_RCA4", "IPSL_CM5A_WRF331F", "MIROC-MIROC5_SMHI-RCA4", "MPI_ESM_RCA4", "MPI_ESM_REMO2009_r1i1p1", "MPI_ESM_REMO2009_r2i1p1", "MPI_RCA4", "SMHI_CCCma_CanESM2_RCA4", "SMHI_NCC_NorESM1_M_RCA4", "SMHI_NOAA_GFDL_GFDL_ESM2M_RCA4")

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
  
  
  data_histo$Date = refTime_histo
  data_rcp45$Date = refTime_rcp45
  data_rcp85$Date = refTime_rcp85
  
  
  climat_histo[[MOD]] = data_histo
  climat_rcp45[[MOD]] = data_rcp45
  climat_rcp85[[MOD]] = data_rcp85
  
}

setNames(climat_histo,MODELS_NAME)
plot(climat_histo[[1]]$SH[1:365])




