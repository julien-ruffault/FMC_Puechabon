# Script for figure 3 : validation of the model on potential and fluxes on puechabon 
# 
# 
# # Initialization ------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# Get simulation results   -----------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
filename  <-  paste0(mainDir,'/Figure_3_validation_potential_fluxes/Potential_and_fluxes_Puechabon_VG.csv')
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')

# Transform simulation results to daily scale for comparaison with fluxes and potential data
DATA_day= setNames(aggregate(DATA$Psi_LSym,by=list(DOY = yday(DATA$Time),YEAR = year(DATA$Time)),FUN= function(x) min(x)),c('DOY','YEAR','Psi_min')) 
DATA_day$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$transpiration   = aggregate(DATA$transpiration_mm,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
DATA_day$PPT= aggregate(DATA$PPT,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x
DATA_day$Tmoy= aggregate(DATA$Tair,by=list(yday(DATA$Time),year(DATA$Time)),mean)$x


# Get sapflow data 
library(readxl)
library(lubridate)
library(hydroGOF) # for function rmse
data_sapflow = as.data.frame(read_excel(paste0(mainDir,'/validation_data/Sapflow_day_SMR_2016-2018.xlsx')))
data_sapflow$Date = as.Date(data_sapflow$Date,format='%Y-%m-%d')
head(data_sapflow)

# Get water potential data (data_MDpot and Data_PDpot)
data_potential = read.csv(paste0(mainDir,'/validation_data/Water_Potential_MIND_Control-1.csv'),dec=',',sep=';')  
data_potential = data_potential[data_potential$Treatment=='Control',] # keep onluy control treatment
data_PDpot     = aggregate(data_potential$Pd_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# moyenne des arbres : conversion en une donne jour pour  le PD_pot et le Md_pot
colnames(data_PDpot) <- c('Date','PDpot_measured')
data_PDpot$Date      <- as.Date(data_PDpot$Date,format='%d/%m/%Y')
data_MDpot = aggregate(data_potential$Md_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# moyenne des arbres : conversion en une données jour par le PD_pot et le Md_pot
colnames(data_MDpot) <- c('Date','MDpot_measured')
data_MDpot$Date <- as.Date(data_MDpot$Date,format='%d/%m/%Y')




# merge datasets by date 
DATA_F = merge(DATA_day,data_PDpot,by='Date',all.x=T)
DATA_F = merge(DATA_F,data_MDpot,by='Date',all.x=T)
DATA_F = merge(DATA_F,data_sapflow[,c('Date','E_Av.gap')],by='Date',all.x=T)
head(DATA_F)
rm(data_potential,data_MDpot,data_PDpot,data_sapflow,DATA_day,DATA) # clean evironment 





# ---- GRAPH ---- 

# options 
cexx=0.7
datesAxis  = seq.Date(from = as.Date("2016/01/01"),to =as.Date("2019/01/01") ,by='3 month')
COL1 = "#3E9651"
COL2 = rgb(171,104,87,maxColorValue=256)


COL5 = "#DA7C30"
COL3 = "#beaed4"
COL4 = gray(.5,1)

COL2B = rgb(218,124,48,150,maxColorValue=256)
COL1B = rgb(62,150,81,150,maxColorValue=256)



graphics.off()
quartz(width=6.3,height=6.5)
plot.new()
par(new=T,plt=c(0.1,0.9,0.7,0.96),xpd=F)
plot(DATA_F$Date,DATA_F$Tmoy,type='n',axes=F,xlab='',ylab='',ylim=c(-10,32),yaxs='i')
abline(v=datesAxis,h=seq(-5,30,5),col=gray(.8,1),lty='dotted')
lines(DATA_F$Date,DATA_F$Tmoy,col=COL5,lwd=.9,lty=1)

axis(1,at=datesAxis,labels=F,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp=c(2,0.5,0))
axis(2,las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0))
mtext(side=2,'Air temperature (°C)',line=1.5,cex=cexx)
box(lwd=.5)
axis(4, las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0),at=seq(-10,30,10),labels=seq(0,240,60))
mtext(side=4,'Precipitation (mm)',line=1.5,cex=cexx)


par(new=T)
plot(DATA_F$Date,DATA_F$PPT,lwd=1.1,type='h',axes=F,xlab='',ylab='',ylim=c(1.8,260),yaxs='i',col=rgb(57,106,203,maxColorValue=256))
mtext('A',adj=0,col=1,side=3,font=2,cex=0.8)



par(new=T,plt=c(0.1,0.9,0.39,0.65),xpd=F)
plot(DATA_F$Date,DATA_F$Psi_min,type='n',axes=F,xlab='',ylab='',ylim=c(-5.5,0.2),yaxs='i')
abline(v=datesAxis,h=seq(-6,0,1),col=gray(.8,1),lty='dotted')
#grid()
lines(DATA_F$Date,DATA_F$Psi_min,col=COL2)
lines(DATA_F$Date,DATA_F$Psi_base,col=COL1)

points(DATA_F$Date,DATA_F$MDpot_measured,bg=COL2B,pch=21,cex=0.8)
points(DATA_F$Date,DATA_F$PDpot_measured,bg=COL1B,pch=21,cex=0.8)

axis(2,las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0))
mtext(side=2,'Water potential (MPa)',line=1.5,cex=cexx)
axis(1,at=datesAxis,labels=F,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp=c(2,0.5,0))
box(lwd=.5)



legend(x= 17480,y=-2.8,bg='white',cex=0.7,  seg.len=1,
       pt.bg = c(COL1B,NA,COL2B,NA),
       lwd=c(1,1.5,1,1.5),
       col = c("black",COL1,"black",COL2),
       box.lwd=0.3,
       pch = c(21,NA,21,NA),
       lty = c(NA,1,NA,1),
       legend= c(expression(paste('Measured ',Psi[lpd])),expression(paste('Simulated ' ,Psi[lpd])), expression(paste('Measured ',Psi[min])) ,expression(paste('Simulated ',Psi[min])))  )

data_eval = na.omit(data.frame(SIM =DATA_F$Psi_min,OBS= DATA_F$MDpot_measured))
R2 = round(summary(lm(data_eval))$r.squared,digit=2)
RMSE = round(rmse(sim =data_eval$SIM ,obs  = data_eval$OBS),digit=2) # midday 

mtext(adj=0.5,col=COL2,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE, ' MPa'),list(R2 = R2,RMSE =RMSE)))
mtext(adj=0.5,col=COL2,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE, ' MPa'),list(R2 = R2,RMSE =RMSE)))

data_eval = na.omit(data.frame(SIM =DATA_F$Psi_base,OBS= DATA_F$PDpot_measured))
R2 = round(summary(lm(data_eval))$r.squared,digit=2)
RMSE = round(rmse(sim =data_eval$SIM ,obs  = data_eval$OBS),digit=2) # midday 

mtext(adj=1,col=COL1,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE, ' MPa'),list(R2 = R2,RMSE =RMSE)))
mtext(adj=1,col=COL1,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE, ' MPa'),list(R2 = R2,RMSE =RMSE)))
mtext('B',adj=0,col=1,side=3,font=2,cex=0.8)



par(new=T,plt=c(0.1,0.9,0.08,0.34),xpd=F)
plot(DATA_F$Date,DATA_F$transpiration,type='n',axes=F,xlab='',ylab='',ylim=c(-0.05,3.2),yaxs='i')


abline(v=datesAxis,h=seq(0,3,.5),col=gray(.8,1),lty='dotted')
points(DATA_F$Date,DATA_F$E_Av.gap,pch=16,cex=.5,col=COL3)
lines(DATA_F$Date,DATA_F$transpiration,col=COL4)

box(lwd=.5)
axis(1,at=datesAxis,labels=format(datesAxis, "%b"),lwd=0,lwd.ticks=0.5,tck=-0.02,mgp=c(2,0.08,0),cex.axis=cexx)
#axis(1, at = datesAxis,labels = format(datesAxis, "%b\n%Y"),cex.axis=cexx)
axis(2,las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0))
#arrows(x0 = 17000, x1 = 17500,y0=-6,y1=-6,lwd=5)  
mtext(side=2,'Transpiration (mm)',line=1.7,cex=cexx)



legend(x= 17450,y=2.95,bg='white',cex=0.7,  seg.len=1,
       lwd=c(1,1.5),
       col = c(COL3,COL4),
       box.lwd=0.3,
       pch = c(16,NA),
       lty = c(NA,1),
       legend= c(expression(paste('Measured ')),expression(paste('Simulated'))))

# add arrows for year
par(xpd=T)
library(shape)
Arrows(x0 = as.numeric(datesAxis[1])+10, x1 = as.numeric(datesAxis[5])-10,y0=-0.52,y1=-0.52,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  
Arrows(x0 = as.numeric(datesAxis[5])+10, x1 = as.numeric(datesAxis[9])-10,y0=-0.52,y1=-0.52,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  
Arrows(x0 = as.numeric(datesAxis[9])+10, x1 = as.numeric(datesAxis[13])-10,y0=-0.52,y1=-0.52,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  

text('2016',y=-0.70,cex=cexx,x =  mean(c(as.numeric(datesAxis[1]), as.numeric(datesAxis[5]))))
text('2017',y=-0.70,cex=cexx,x =  mean(c(as.numeric(datesAxis[5]), as.numeric(datesAxis[9]))))                                                                  
text('2018',y=-0.70,cex=cexx,x =  mean(c(as.numeric(datesAxis[9]), as.numeric(datesAxis[13]))))

data_eval = na.omit(data.frame(SIM =DATA_F$transpiration,OBS= DATA_F$E_Av.gap))
R2 = round(summary(lm(data_eval))$r.squared,digit=2)
RMSE = round(rmse(sim =data_eval$SIM ,obs  = data_eval$OBS),digit=2) # midday 

mtext(adj=1,col=COL3,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE),list(R2 = R2,RMSE =RMSE)))
mtext(adj=1,col=COL3,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE),list(R2 = R2,RMSE =RMSE)))

mtext('C',adj=0,col=1,side=3,font=2,cex=0.8)

# par(new=T)
# plot(DATA_F$Date, DATA_F$PPT,type='h', ylim=c(1,270), ylab="", xlab="", las=1,axes=F,yaxs='i',col='dodgerblue3')
# #lines(DATA_day$Date, DATA_day$PLCleaf,col='firebrick3')
# #axis(4, las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0),at=seq(5,260,40),labels=seq(0,240,40))
# mtext(side=4,'Precipitation (mm)',line=1.5,cex=cexx)
