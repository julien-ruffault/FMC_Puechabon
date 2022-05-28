#---------------------------------------------------------------------------------------
# Script for plottinng Figure 4 of LFMC Puechabon pape : LFMC validation 
# Date : 02/02/2022 (!)
# Authors : nicolas martin (nicolas.martin@inrae.fr)
#           julien ruffault (julien.ruffault@inrae.fr)
#---------------------------------------------------------------------------------------

# The script read the simulations runs previously written by another script (Launcher_Puechabon_VG_FMC_validFMC)
# the script plot the figure 3


# Plot sim with measured parameters
# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  

#Data reading
datPsiFMC = read.csv(paste0(mainDir,"/validation_data/FMC Puéchabon 2016-2018.csv"), sep=";", dec=",", h=T)
datPsiFMC = datPsiFMC[-c(13,16,18),]
datPsiFMC = datPsiFMC[datPsiFMC[,"Include"]==1,]
datadate = as.Date(strptime(datPsiFMC[,"Date"],format="%d/%m/%Y"))
datPsiFMC$Date <-datadate

#SurEau outputs reading
output_path_measured <-  paste0(mainDir,'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_Measured.csv')
DATA      = read.csv(output_path_measured,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
DATA$Date = as.Date(DATA$Time,format='%j/%Y')

#Converting hourly simulations data to daily 
DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$LFMC_base = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$LFMC_min = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$LFMC_apo = aggregate(DATA$LFMCApo,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$LFMC_sym = aggregate(DATA$LFMCSymp,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')

simu_DD = merge(datPsiFMC, DATA_day,  by='Date', all.x=T)


quartz()
par(new=F, plt=c(0.1,0.45,0.55,0.90), las=1, tck=0.02)
plot(DATA_day$Date,DATA_day$LFMC_base,type='l',col=1, ylim=c(30,85), ylab="", xlab="", xaxt="n")
lines(DATA_day$Date,DATA_day$LFMC_min,type='l',col=2)
points(datadate, datPsiFMC$FMC_Pd.Control*100, pch=21, bg=adjustcolor(1, 0.4))
points(datadate, datPsiFMC$FMC_Md.Control*100, col=1, pch=21, bg=adjustcolor(2, 0.4))
mtext("measured parameters", side=3)

par(new=T, plt=c(0.15, 0.23,0.57,0.65), tck=0.02, las=1)
LFMCPd=(simu_DD$FMC_Pd.Control)*100
LFMCMd=simu_DD$FMC_Md.Control*100
plot(simu_DD$LFMC_base~LFMCPd,pch=21, ylim=c(45, 85), xlim=c(45,85), bg=adjustcolor(1,alpha=.5), ylab="", xlab="",yaxt="n", xaxt="n")
points(simu_DD$LFMC_min~LFMCMd,pch=21, bg=adjustcolor(2,alpha=.5))
abline(lm(simu_DD$LFMC_base~LFMCPd))
abline(lm(simu_DD$LFMC_min~LFMCMd), col=2)
abline(0,1, lwd=2)
axis(1,  mgp=c(0,-0.2,0), cex.axis=.5)
axis(2,  mgp=c(0,0.1,0), cex.axis=.5)


# Adjusted all years
output_path_adjAll <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_AdjustedAllYears.csv')
DATA      = read.csv(output_path_adjAll,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$LFMC_base = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$LFMC_min = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$LFMC_apo = aggregate(DATA$LFMCApo,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$LFMC_sym = aggregate(DATA$LFMCSymp,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
simu_DD = merge(datPsiFMC, DATA_day,  by='Date', all.x=T)

par(new=T, plt=c(0.6,0.95,0.55,0.90)) 
plot(DATA_day$Date,DATA_day$LFMC_base,type='l',col=1, ylim=c(30,85), ylab="", xlab="", xaxt="n")
lines(DATA_day$Date,DATA_day$LFMC_min,type='l',col=2)
points(datadate, datPsiFMC$FMC_Pd.Control*100, pch=21, bg=adjustcolor(1, 0.4))
points(datadate, datPsiFMC$FMC_Md.Control*100, col=1, pch=21, bg=adjustcolor(2, 0.4))
LFMCPd=(simu_DD$FMC_Pd.Control)*100
LFMCMd=simu_DD$FMC_Md.Control*100
mtext("Calibrated (all year)", side=3)

par(new=T, plt=c(0.65,0.73,0.57,0.65), tck=0.02, las=1)
plot(simu_DD$LFMC_base~LFMCPd,pch=21, ylim=c(45, 85), xlim=c(45,85), bg=adjustcolor(1,alpha=.5), ylab="", xlab="",yaxt="n", xaxt="n")
points(simu_DD$LFMC_min~LFMCMd,pch=21, bg=adjustcolor(2,alpha=.5))
abline(lm(simu_DD$LFMC_base~LFMCPd))
abline(lm(simu_DD$LFMC_min~LFMCMd), col=2)
abline(0,1, lwd=2)
axis(1,  mgp=c(0,-0.2,0), cex.axis=.5)
axis(2,  mgp=c(0,0.1,0), cex.axis=.5)


# Adjusted per year
output_path_adj_2016 <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_Adjusted_2016.csv')
output_path_adj_2017 <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_Adjusted_2017.csv')
output_path_adj_2018 <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_Adjusted_2018.csv')
DATA2016      = read.csv(output_path_adj_2016,header=T, dec='.', sep="")
DATA2017      = read.csv(output_path_adj_2017,header=T, dec='.', sep="")
DATA2018      = read.csv(output_path_adj_2018,header=T, dec='.', sep="")
DATA          = rbind.data.frame(DATA2016,DATA2017,DATA2018)

DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$LFMC_base = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$LFMC_min = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$LFMC_apo = aggregate(DATA$LFMCApo,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$LFMC_sym = aggregate(DATA$LFMCSymp,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')

simu_DD = merge(datPsiFMC, DATA_day,  by='Date', all.x=T)

par(new=T, plt=c(0.1,0.45,0.15,0.5)) 
plot(DATA_day$Date,DATA_day$LFMC_base,type='l',col=1, ylim=c(30,85), ylab="", xlab="")
lines(DATA_day$Date,DATA_day$LFMC_min,type='l',col=2)
points(datadate, datPsiFMC$FMC_Pd.Control*100, pch=21, bg=adjustcolor(1, 0.4))
points(datadate, datPsiFMC$FMC_Md.Control*100, col=1, pch=21, bg=adjustcolor(2, 0.4))
mtext("Calibrated (per year)", side=3)

par(new=T, plt=c(0.15, 0.23,0.2,0.28), tck=0.02, las=1)
plot(simu_DD$LFMC_base~LFMCPd,pch=21, ylim=c(45, 85), xlim=c(45,85), bg=adjustcolor(1,alpha=.5), ylab="", xlab="",yaxt="n", xaxt="n")
points(simu_DD$LFMC_min~LFMCMd,pch=21, bg=adjustcolor(2,alpha=.5))
abline(lm(simu_DD$LFMC_base~LFMCPd))
abline(lm(simu_DD$LFMC_min~LFMCMd), col=2)
abline(0,1, lwd=2)
axis(1,  mgp=c(0,-0.2,0), cex.axis=.5)
axis(2,  mgp=c(0,0.1,0), cex.axis=.5)

#Symp and apo dynamics (Adjusted all years)

# 
output_path_adjAll <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_AdjustedAllYears.csv')
DATA      = read.csv(output_path_adjAll,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
par(new=T, plt=c(0.6,0.95,0.15,0.5)) 
plot(DATA$Time, DATA$LFMCSymp, type='l',xlab="", col="orange", ylab="", ylim=c(0, 80))
lines(DATA$Time, DATA$LFMCApo, type='l', col=2)
mtext("Dynamic calibrated (all year)", side=3)
legend(DATA$Time[1],20, c("apoplasmic", "symplasmic"), lty=1, col=c("red", "orange"), bty="n")




