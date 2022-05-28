# ---------------------------------------------------------------------------------------
# -- Figure 5 LFMC paper : (Canopy scale water content)
# ---------------------------------------------------------------------------------------
# RCode for plottinng Figure 5 of LFMC Puechabon paper.
# The script read the simulations runs previously written by another script (Launcher_Puechabon_VG_FMC_validFMC)
# the script plot the figure 3
# -----------------------------------------


rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
#
library("png")
#-----------------------------------------------------
#Read model outputs : "adjusted all years Puechabon.
output_path_adjAll <-  paste0(mainDir,'/scripts_base_simulations/Puechabon_VG_LFMC_AdjustedAllYears.csv')
DATA      = read.csv(output_path_adjAll,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')

#-----------------------------------------------------
# Read an combine the  NDVI data
#-----------------------------------------------------
pp <- readPNG(paste0(mainDir,"/Figure_5_validataion_FMC/defol.png"))

datNDVI=read.csv("validation_data/NDVI-PRI_2013-2019.csv",  sep=";", dec=".", h=T, skip=1)
dateNDVI=as.Date(strptime(datNDVI$Date, format="%d/%m/%Y"))
yearNDVI=as.numeric(strftime(dateNDVI,  format='%Y'))
doyNDVI=as.numeric(strftime(dateNDVI,  format='%j'))
vecSelNDVI=yearNDVI %in% c("2016", "2017", "2018")

datNDVISel=datNDVI[vecSelNDVI,]
doyNDVISel=as.numeric(strftime(dateNDVI[vecSelNDVI],  format='%j'))
YearNDVISel=as.numeric(strftime(dateNDVI[vecSelNDVI],  format='%Y'))

filtdoy= doyNDVISel <163 | doyNDVISel>275
NDVINew=datNDVISel[,4]
NDVINew[filtdoy]=NA

dayD=190 # From early July (leaf maturity @Puechabon) : DOY for NDVI leaf maturity but before drought 07/07
dayF=270  #To end september early october (: )End of drought) :DOY pour le NDVI après sécheresse 30/09

Dat2016=NDVINew[YearNDVISel==2016]
mNDVIdeb=Dat2016[(dayD-7):(dayD+7)]
mNDVIfin=Dat2016[(dayF-7):(dayF+7)]
i=0
difres2016=NULL
while(i<1000) {i=i+1 ; difres2016=c(difres2016,mean(sample(mNDVIdeb, 10)- sample(mNDVIfin, 10))/(0.67-0.3))}
difres2016=cbind.data.frame(year=2016, var= difres2016)
PLN2016=cbind.data.frame(year=2016, var=(mNDVIdeb-mNDVIfin)/(0.67-0.3))

Dat2017=NDVINew[YearNDVISel==2017]
mNDVIdeb=Dat2017[(dayD-7):(dayD+7)]
mNDVIfin=Dat2017[(dayF-7):(dayF+7)]
PLN2017=cbind.data.frame(year=2017, var=(mNDVIdeb-mNDVIfin)/(0.67-0.3) )
mNDVIfin=mNDVIfin[!is.na(mNDVIfin)]
difres2017=NULL
i=0
while(i<1000) {i=i+1 ; 
temp=mean((sample(mNDVIdeb, 10)  - sample(mNDVIfin, 10))/(0.67-0.3))
difres2017=c(difres2017,temp)
}
difres2017=cbind.data.frame(year=2017, var= difres2017)

Dat2018=NDVINew[YearNDVISel==2018]
mNDVIdeb=Dat2018[(dayD-7):(dayD+7)]
mNDVIfin=Dat2018[(dayF-7):(dayF+7)]
PLN2018=cbind.data.frame(year=2018, var=(mNDVIdeb-mNDVIfin)/(0.67-0.3))
sumNDVI=rbind(PLN2016,PLN2017,PLN2018)
difres2018=NULL
i=0
while(i<1000) {i=i+1 ; 
temp=mean((sample(mNDVIdeb, 10)  - sample(mNDVIfin, 10))/(0.67-0.3))
difres2018=c(difres2018,temp)
}
difres2018=cbind.data.frame(year=2018, var= difres2018)

DIFRESNDVI=rbind(difres2016,difres2017,difres2018)

#----------------------------------
#Plot the plots
DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$LFMC_min = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$FMC_min = aggregate(DATA$FMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
DATA_day$LAIdead = aggregate(DATA$LAIdead,by=list(yday(DATA$Time),year(DATA$Time)),min)$x

plot(DATA_day$LFMC_min, type='l')
lines(DATA_day$FMC_min, col=2)

Vend = length(DATA$Time)

quartz()
par(new=F, plt=c(0.1,0.42,0.58,0.90), tck=.02)
plot(DATA_day$LFMC_min~DATA_day$Date, type='p',pch="", ylim=c(30,85), ylab="",mgp=c(0,0.3,0) , xlab="", las=1, yaxt="n")
polygon(x=c(DATA_day$Date, rev(DATA_day$Date)), y=c(DATA_day$LFMC_min, rev(DATA_day$FMC_min)), col=adjustcolor(col="orange", alpha.f = .7), lwd=.1)
lines(DATA_day$LFMC_min~DATA_day$Date, col=adjustcolor(col="dark green", alpha.f = 0.3), lwd=.5)
lines(DATA_day$FMC_min~DATA_day$Date, col="dark grey", lwd=.4)
mtext("(A)", 3, line=0, adj=0)
axis(2, col.axis=1, las=1, col=1, mgp=c(0,.5,0))
mtext("LFMC and FMC canopy", side=2.5, col=1, line=2, cex=.8)
par(new=T)
plot(DATA_day$LAIdead~DATA_day$Date, type='l', ylim=c(0,2), ylab="", mgp=c(0,0.3,0) , xlab="", las=1, xaxt="n" ,yaxt="n", col=2)
axis(4, col.axis=2, las=1, col=1, mgp=c(0,.5,0))
mtext("canopy mortality", side=4, col=2, line=2, cex=.8)


par(new=T, plt=c(0.6,0.92,0.58,0.90), tck=.02)
boxplot(DIFRESNDVI$var~DIFRESNDVI$year, yaxt="n", ylim=c(0, 0.5), col=adjustcolor(2, alpha.f = .5), xaxt="n", tck="n", ylab='', xlab="")
mtext("(B)", 3, line=0, adj=0)
axis(2, col.axis=2, las=1, col=2 , mgp=c(0,.5,0))
mtext("relative loss NDVI", side=2.5, col=2, line=2, cex=.8)
par(new=T)
plot(NDVINew~dateNDVI[vecSelNDVI], ylim=c(0.3,.7), type="l",ylab="", yaxt="n", las=1, tck=.02, xlab="")
axis(4, las=1, mgp=c(0,.5,0))
mtext("NDVI", side=4, line=1.5, cex=.8)
par(new=T,plt=c(0.5,1,0.1,0.44), tck=.02)

par(new=T,plt=c(0.1,0.82,0.1,0.45), tck=.02)
plot.new()
rasterImage(pp, 0,0,1,1)
mtext("end summer 2017", side=3)
mtext("(C)", 3, line=0, adj=0)


