# authors : julien ruffault (julien.ruff@gmail.com)
# Date : 26/01/2022
# Data formatting  and computing dervied statistics for FMC projections from climate models 


rm(list=ls())
gc()

# Set paths 
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  

# model selection by Pimont et al. 
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



histo_FMC = 

for (MOD in 1:length(MODELS_NAME))
{
  
 histo_FMC 
  
}  