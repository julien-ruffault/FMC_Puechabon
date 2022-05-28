
#---------------------------------------------------------------------------------------
# Script for plottinng Figure 5 of LFMC Puechabon pape :  validation on FMCcan
# Date : 02/02/2022 (!)
# Authors : nicolas martin (nicolas.martin@inrae.fr)
#           julien ruffault (julien.ruffault@inrae.fr)


rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
#
library("png")
#-----------------------------------------------------
#Read model outputs : "adjusted all years Puechabon.
#output_path_adjAll <-  paste0(mainDir,'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_AdjustedAllYears.csv')
#DATA      = read.csv(output_path_adjAll,header=T, dec='.', sep="")

#SurEau outputs reading (Adjusted all years)
output1 <-  paste0(mainDir,'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_Adjusted_2016.csv')
DATA1      = read.csv(output1,header=T, dec='.', sep="")

output2 <-  paste0(mainDir,'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_Adjusted_2017.csv')
DATA2      = read.csv(output2,header=T, dec='.', sep="")

output3 <-  paste0(mainDir,'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_Adjusted_2018.csv')
DATA3      = read.csv(output3,header=T, dec='.', sep="")

DATA = rbind(DATA1,DATA2,DATA3)


DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')

DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$LFMC_min = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$FMC_min = aggregate(DATA$FMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
DATA_day$LAIdead = aggregate(DATA$LAIdead,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
DATA_day$PLCleaf = aggregate(DATA$PLC_Leaf,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
# plot(DATA_day$LFMC_min, type='l')
# lines(DATA_day$FMC_min, col=2)
# 
# #-----------------------------------------------------
# # Read an combine the  NDVI data
# #-----------------------------------------------------
# pp <- readPNG(paste0(mainDir,"/Figure_5_validation_CanopyMC/defol.png"))
# 
# datNDVI=read.csv(paste0(mainDir,"/validation_data/NDVI-PRI_2013-2019.csv"),  sep=";", dec=".", h=T, skip=1)
# dateNDVI=as.Date(strptime(datNDVI$Date, format="%d/%m/%Y"))
# yearNDVI=as.numeric(strftime(dateNDVI,  format='%Y'))
# doyNDVI=as.numeric(strftime(dateNDVI,  format='%j'))
# vecSelNDVI=yearNDVI %in% c("2016", "2017", "2018")
# 
# datNDVISel=datNDVI[vecSelNDVI,]
# doyNDVISel=as.numeric(strftime(dateNDVI[vecSelNDVI],  format='%j'))
# YearNDVISel=as.numeric(strftime(dateNDVI[vecSelNDVI],  format='%Y'))
# 
# filtdoy= doyNDVISel <163 | doyNDVISel>275
# NDVINew=datNDVISel[,4]
# NDVINew[filtdoy]=NA
# 
# dayD=190 # From early July (leaf maturity @Puechabon) : DOY for NDVI leaf maturity but before drought 07/07
# dayF=270  #To end september early october (: )End of drought) :DOY pour le NDVI après sécheresse 30/09
# 
# Dat2016=NDVINew[YearNDVISel==2016]
# mNDVIdeb=Dat2016[(dayD-7):(dayD+7)]
# mNDVIfin=Dat2016[(dayF-7):(dayF+7)]
# i=0
# difres2016=NULL
# while(i<1000) {i=i+1 ; difres2016=c(difres2016,mean(sample(mNDVIdeb, 10)- sample(mNDVIfin, 10))/(0.67-0.3))}
# difres2016=cbind.data.frame(year=2016, var= difres2016)
# PLN2016=cbind.data.frame(year=2016, var=(mNDVIdeb-mNDVIfin)/(0.67-0.3))
# 
# Dat2017=NDVINew[YearNDVISel==2017]
# mNDVIdeb=Dat2017[(dayD-7):(dayD+7)]
# mNDVIfin=Dat2017[(dayF-7):(dayF+7)]
# PLN2017=cbind.data.frame(year=2017, var=(mNDVIdeb-mNDVIfin)/(0.67-0.3) )
# mNDVIfin=mNDVIfin[!is.na(mNDVIfin)]
# difres2017=NULL
# i=0
# while(i<1000) {i=i+1 ; 
# temp=mean((sample(mNDVIdeb, 10)  - sample(mNDVIfin, 10))/(0.67-0.3))
# difres2017=c(difres2017,temp)
# }
# difres2017=cbind.data.frame(year=2017, var= difres2017)
# 
# Dat2018=NDVINew[YearNDVISel==2018]
# mNDVIdeb=Dat2018[(dayD-7):(dayD+7)]
# mNDVIfin=Dat2018[(dayF-7):(dayF+7)]
# PLN2018=cbind.data.frame(year=2018, var=(mNDVIdeb-mNDVIfin)/(0.67-0.3))
# sumNDVI=rbind(PLN2016,PLN2017,PLN2018)
# difres2018=NULL
# i=0
# while(i<1000) {i=i+1 ; 
# temp=mean((sample(mNDVIdeb, 10)  - sample(mNDVIfin, 10))/(0.67-0.3))
# difres2018=c(difres2018,temp)
# }
# difres2018=cbind.data.frame(year=2018, var= difres2018)
# 
# DIFRESNDVI=rbind(difres2016,difres2017,difres2018)



# 
# 
# 
# DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
# colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
# DATA_day$LFMC_min = aggregate(DATA$LFMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
# DATA_day$FMC_min = aggregate(DATA$FMC,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
# DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
# DATA_day$LAIdead = aggregate(DATA$LAIdead,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
# DATA_day$PLCleaf = aggregate(DATA$PLC_Leaf,by=list(yday(DATA$Time),year(DATA$Time)),min)$x
# # plot(DATA_day$LFMC_min, type='l')
# # lines(DATA_day$FMC_min, col=2)
# # 
# Vend = length(DATA$Time)


#----------------------------------
# options 
cexx=0.7
datesAxis   = seq.Date(from = as.Date("2016/01/01"),to =as.Date("2019/01/01") ,by='3 month')
datesAxisH  = seq.POSIXt(from = as.POSIXct("2016/01/01"),to =as.POSIXct("2019/01/01") ,by='3 month')
COL1 = "#3E9651"
COL2 = "#DA7C30"
#COL3 = "#396AB1"

COL2B = rgb(218,124,48,150,maxColorValue=256)
COL1B = rgb(62,150,81,150,maxColorValue=256)

COL3 = gray(0.4)
COL4 = rgb(171,104,87,maxColorValue=256)


#Plot the plots


quartz(width=6.3,height=2.5)
plot.new()
par(new=T,plt=c(0.1,0.9,0.55,0.95),xpd=F)
plot(DATA_day$Date,DATA_day$LFMC_min,type='n',col=1, ylim=c(38,81), ylab="", xlab="",axes=F,yaxs='i')
abline(v=datesAxis,h=seq(30,80,10),col=gray(.8,1),lty='dotted')
#polygon(x=c(DATA_day$Date, rev(DATA_day$Date)), y=c(DATA_day$LFMC_min, rev(DATA_day$FMC_min)), col=rgb(218,124,48,150,maxColorValue=256),border=NA)
polygon(x=c(DATA_day$Date, rev(DATA_day$Date)), y=c(DATA_day$LFMC_min, rev(DATA_day$FMC_min)), col=gray(0.5,0.5),border=NA)
lines(DATA_day$Date,DATA_day$FMC_min,col=1)
lines(DATA_day$Date,DATA_day$LFMC_min,col=COL2)

axis(2,las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0))
mtext(side=2,'Fuel moisture content (%)',line=2,cex=cexx)
box(lwd=0.5)






par(new=T)
plot(DATA_day$Date, DATA_day$PLCleaf,type='n', ylim=c(-5,102.5), ylab="", xlab="", las=1,axes=F,yaxs='i')
lines(DATA_day$Date, DATA_day$PLCleaf,col='firebrick3')
axis(4, las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0),at=seq(0,100,25))
mtext(side=4,'Percent  of conductance (%)',line=1.5,cex=cexx)
#mtext("canopy mortality", side=4, col=2, line=2, cex=.8)

legend(x= datesAxis[10],y=40,bg='white',cex=0.7,  seg.len=1,
       lwd=1.5,
       col = c(COL2,1,'firebrick3'),
       box.lwd=0.3,
       lty = 1,
       legend= c(expression(paste(LFMC[min])),expression(paste(FMC[Can])), expression(paste('Cavitation'))))



axis(1,at=datesAxis,labels=format(datesAxis, "%b"),lwd=0,lwd.ticks=0.5,tck=-0.02,mgp=c(2,0.08,0),cex.axis=cexx)
#axis(1, at = datesAxis,labels = format(datesAxis, "%b\n%Y"),cex.axis=cexx)

# add arrows for year
par(xpd=T)
library(shape)
Arrows(x0 = as.numeric(datesAxis[1])+10, x1 = as.numeric(datesAxis[5])-10,y0=-21,y1=-21,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  
Arrows(x0 = as.numeric(datesAxis[5])+10, x1 = as.numeric(datesAxis[9])-10,y0=-21,y1=-21,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  
Arrows(x0 = as.numeric(datesAxis[9])+10, x1 = as.numeric(datesAxis[13])-10,y0=-21,y1=-21,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  


text('2016',y=-27,cex=cexx,x =  mean(c(as.numeric(datesAxis[1]), as.numeric(datesAxis[5]))))
text('2017',y=-27,cex=cexx,x =  mean(c(as.numeric(datesAxis[5]), as.numeric(datesAxis[9]))))                                                                   
text('2018',y=-27,cex=cexx,x =  mean(c(as.numeric(datesAxis[9]), as.numeric(datesAxis[13]))))                                                                   


mtext('A',adj=0,col=1,side=3,font=2,cex=0.8)




