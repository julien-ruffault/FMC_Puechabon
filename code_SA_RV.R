# Sensitivity analysis for SurEau-Ecos
#Authors : Julien Ruffault (julien.ruff@gmail.com)
# Date   : 24/01/2022

library(sensobol)


# clean environment
rm(list=ls(all=TRUE));gc()

# define working directory as the directory of the script
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  

# Define general parameters for the simulation <-- add or remove parameters here
params=c('P50_VC_Leaf',
         'gmin20',
         "epsilon_Sym",
         "PiFullTurgor",
         "Q10_1_gmin",
         "TPhase_gmin",
         "apoFrac_Leaf",
         'LAImax',
         'SWC',
         'P50_gs',
         'LDMC')

percentV = 20/100 # percentage of variations around each parameters
N <- 1000 # 1000 is a reduced number simulations for tests  / number is  about 30000-50000 for full models
k <- length(params) # number of parameters
R <- 10^3 
type <- "norm"
conf <- 0.95

# built sampling matrix (pseudo random)
PARAMS <- sobol_matrices(params = params, N = N, order = "first")

# sampling for each parameter around its nominal value
PARAMS[, "LAImax"]       <- qunif(PARAMS[, "LAImax"], 2.2 - percentV * 2.2, 2.2 + percentV * 2.2)
PARAMS[, "P50_VC_Leaf"]  <- qunif(PARAMS[, "P50_VC_Leaf"] , -4+ -4* percentV, -4- -4* percentV)
PARAMS[, "gmin20"]       <- qunif(PARAMS[, "gmin20"]      , 3- percentV * vegFile$gmin20, 3+ percentV * vegFile$gmin20)
PARAMS[, "P50_gs"]       <- qunif(PARAMS[, "P50_gs"]      , -2+ -2* percentV, -2- -2* percentV)
###... continue the sampling for each chosen parameter





write.csv(PARAMS,paste0(Out_dir,'/PARAMS_.csv'),row.names=F)

#-- --- -- - - - - - -- -
##########################################
### RUN SUREAU WITH THESE PARAMETERS ####
##########################################
##########################################
#-- 
#
#
#
#

# Calculate sobol indices 
Y1 = NULL
for (i in 1:nrow(PARAMS))
{
  io =read.csv(paste0(Out_dir,'/SA_FMC_',i,'.csv'),header=T, dec='.',sep="") # read output
  Y1[i]  = io$yearly_FMCCanopy_min # just an example 
}

A <- sobol_indices(Y = Y1, N = N, params = params, order="first",boot = T, R=R,conf=conf,type=type)
plot(A)
plot_scatter(Y = Y1, N = N, data = PARAMS, params =params)

