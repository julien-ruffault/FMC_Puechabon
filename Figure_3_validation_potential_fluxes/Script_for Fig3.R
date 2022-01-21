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



# Get sapflow data 
library(readxl)
library(lubridate)
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
head(data_PDpot)
head(data_MDpot)


# merge datasets by date 
DATA_F = merge(DATA_day,data_PDpot,by='Date',all.x=T)
DATA_F = merge(DATA_F,data_MDpot,by='Date',all.x=T)
DATA_F = merge(DATA_F,data_sapflow[,c('Date','E_Av.gap')],by='Date',all.x=T)
head(DATA_F)
rm(data_potential,data_MDpot,data_PDpot,data_sapflow,DATA_day,DATA) # clean evironment 





# ---- GRAPH ---- 

# options 
cexx=0.7
datesAxis  = seq.Date(from = as.Date("2016/01/15"),to =as.Date("2018/12/15") ,by='3 month')
COL1 = "#3E9651"
COL2 = "#DA7C30"
COL3 = "#396AB1"

COL1B = rgb(218,124,48,90,maxColorValue=256)
COL2B = rgb(62,150,81,90,maxColorValue=256)



graphics.off()
quartz(width=6.3,height=4.5)
plot.new()
par(new=T,plt=c(0.1,0.95,0.55,0.95))
plot(DATA_F$Date,DATA_F$Psi_min,type='n',axes=F,xlab='',ylab='')
abline(v=datesAxis,h=seq(-5,0,1),col=gray(.8,1),lty='dotted')
#grid()
lines(DATA_F$Date,DATA_F$Psi_min,col=COL2)
lines(DATA_F$Date,DATA_F$Psi_base,col=COL1)

points(DATA_F$Date,DATA_F$MDpot_measured,bg=COL1B,pch=21,cex=0.8)
points(DATA_F$Date,DATA_F$PDpot_measured,bg=COL2B,pch=21,cex=0.8)

axis(2,las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02)
axis(1,at=datesAxis,labels=F,lwd=0,lwd.ticks=0.5,tck=-0.02)

#axis(1, at = seq.Date(from = as.Date("2016/01/01"),to =as.Date("2018/12/1") ,by='month'))
box(lwd=.5)


par(new=T,plt=c(0.15,0.25,0.6,0.65))
plot(1:10,axes=F,xlab="",ylab="")
box()


par(new=T,plt=c(0.1,0.95,0.1,0.5))
plot(DATA_F$Date,DATA_F$transpiration,type='n',axes=F,xlab='',ylab='',ylim=c(0,3.2))
abline(v=datesAxis,h=seq(0,3,.5),col=gray(.8,1),lty='dotted')
lines(DATA_F$Date,DATA_F$transpiration,col=COL3)
points(DATA_F$Date,DATA_F$E_Av.gap,pch=16,cex=.5,col=gray(.1,.5))
box(lwd=.5)


#datesAxis  = seq.Date(from = as.Date("2016/01/15"),to =as.Date("2018/12/15") ,by='2 month')
axis(1, at = datesAxis,labels = format(datesAxis, "%b\n%Y"),cex.axis=cexx)
axis(2,las=2,cex.axis=cexx)


s
