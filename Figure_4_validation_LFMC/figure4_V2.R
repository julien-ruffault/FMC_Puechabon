#---------------------------------------------------------------------------------------
# Script for plottinng Figure 4 of LFMC Puechabon pape : LFMC validation 
# Date : 02/02/2022 (!)
# Authors : nicolas martin (nicolas.martin@inrae.fr)
#           julien ruffault (julien.ruffault@inrae.fr)
#---------------------------------------------------------------------------------------

# The script read the simulations runs previously written by another script (Launcher_Puechabon_VG_FMC_validFMC)


# Plot sim with measured parameters
# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  

# reading data fro LFMC measurements 
datPsiFMC = read.csv(paste0(mainDir,"/validation_data/FMC Puéchabon 2016-2018.csv"), sep=";", dec=",", h=T)
datPsiFMC = datPsiFMC[-c(13,16,18),]
datPsiFMC = datPsiFMC[datPsiFMC[,"Include"]==1,]
datadate = as.Date(strptime(datPsiFMC[,"Date"],format="%d/%m/%Y"))
datPsiFMC$Date <-datadate

#SurEau outputs reading (Adjusted all years)
output_path_adjAll <-  paste0(mainDir,'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_AdjustedAllYears.csv')
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


# GRAPH 

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


quartz(width=6.3,height=4.5)
plot.new()
par(new=T,plt=c(0.1,0.95,0.56,0.95),xpd=F)
plot(DATA_day$Date,DATA_day$LFMC_base,type='n',col=1, ylim=c(38,81), ylab="", xlab="",axes=F,yaxs='i')
abline(v=datesAxis,h=seq(30,80,10),col=gray(.8,1),lty='dotted')
#grid()
lines(DATA_day$Date,DATA_day$LFMC_base,col=COL1)
lines(DATA_day$Date,DATA_day$LFMC_min,col=COL2)

points(datadate, datPsiFMC$FMC_Pd.Control*100, pch=22, bg=COL1B,cex=0.8)
points(datadate[-1], datPsiFMC$FMC_Md.Control[-1]*100, pch=22, bg=COL2B,cex=0.8)


axis(2,las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0))
mtext(side=2,'Fuel moisture content (%)',line=1.5,cex=cexx)
axis(1,at=datesAxis,labels=F,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp=c(2,0.5,0))
box(lwd=.5)

legend(x= datesAxis[9],y=56.15,bg='white',cex=0.7,  seg.len=1,
       pt.bg = c(COL1B,NA,COL2B,NA),
       lwd=c(1,1.5,1,1.5),
       col = c("black",COL1,"black",COL2),
       box.lwd=0.3,
       pch = c(22,NA,22,NA),
       lty = c(NA,1,NA,1),
       legend= c(expression(paste('Measured ',LFMC[max])),expression(paste('Simulated ' ,LFMC[max])), expression(paste('Measured ',LFMC[min])) ,expression(paste('Simulated ',LFMC[min])))  )

AAA = merge(data.frame(Date= datadate,OBS =datPsiFMC$FMC_Pd.Control*100), DATA_day[,c('Date','LFMC_base')])
data_eval = na.omit(data.frame(SIM =AAA$LFMC_base,OBS= AAA$OBS))
R2 = round(summary(lm(data_eval))$r.squared,digit=2)
RMSE = round(rmse(sim =data_eval$SIM ,obs  = data_eval$OBS),digit=2) # midday 

mtext(adj=1,col=COL1,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE),list(R2 = R2,RMSE =RMSE)))
mtext(adj=1,col=COL1,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE),list(R2 = R2,RMSE =RMSE)))

AAA = merge(data.frame(Date= datadate,OBS =datPsiFMC$FMC_Md.Control*100), DATA_day[,c('Date','LFMC_min')])
data_eval = na.omit(data.frame(SIM =AAA$LFMC_min,OBS= AAA$OBS))
R2 = round(summary(lm(data_eval))$r.squared,digit=2)
RMSE = round(rmse(sim =data_eval$SIM ,obs  = data_eval$OBS),digit=2) # midday 

mtext(adj=0.5,col=COL2,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE),list(R2 = R2,RMSE =RMSE)))
mtext(adj=0.5,col=COL2,side=3,font=2,cex=cexx,substitute(paste(italic(R^2),'=',R2,' ; RMSE=',RMSE),list(R2 = R2,RMSE =RMSE)))


mtext('A',adj=0,col=1,side=3,font=2,cex=0.8)




par(new=T,plt=c(0.1,0.95,0.1,0.49),xpd=F)
plot(DATA$Time, DATA$LFMCSymp, type='n', col="orange",  ylim=c(38,81), ylab="", xlab="",axes=F,yaxs='i')
abline(v=datesAxisH,h=seq(30,80,10),col=gray(.8,1),lty='dotted')
box(lwd=0.5)

lines(DATA$Time, DATA$LFMCSymp,col=COL3,lwd=.6)
lines(DATA$Time, DATA$LFMCApo, type='l', col=COL4,lwd=1)
axis(2,las=2,cex.axis=cexx,lwd=0,lwd.ticks=0.5,tck=-0.02,mgp = c(2,0.5,0))
mtext(side=2,'Fuel moisture content (%)',line=1.5,cex=cexx)
axis(1,at=datesAxisH,labels=format(datesAxisH, "%b"),lwd=0,lwd.ticks=0.5,tck=-0.02,mgp=c(2,0.08,0),cex.axis=cexx)

legend(x= datesAxisH[9],y=50,bg='white',cex=0.7,  seg.len=1,
       lwd=c(1.5,1.5),
       col = c(COL3,COL4),
       box.lwd=0.3,
       lty = 1,
       legend= c("Symplasm", "Apoplasm"))

# add arrows for year
par(xpd=T)
library(shape)
Arrows(x0 = as.numeric(datesAxisH[1])+600000, x1 = as.numeric(datesAxisH[5])-600000,y0=32,y1=32,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  
Arrows(x0 = as.numeric(datesAxisH[5])+600000, x1 = as.numeric(datesAxisH[9])-600000,y0=32,y1=32,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  
Arrows(x0 = as.numeric(datesAxisH[9])+600000, x1 = as.numeric(datesAxisH[13])-600000,y0=32,y1=32,lwd=0.8,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12)  


text('2016',y=29.5,cex=cexx,x =  mean(c(as.numeric(datesAxisH[1]), as.numeric(datesAxisH[5]))))
text('2017',y=29.5,cex=cexx,x =  mean(c(as.numeric(datesAxisH[5]), as.numeric(datesAxisH[9]))))                                                                   
text('2018',y=29.5,cex=cexx,x =  mean(c(as.numeric(datesAxisH[9]), as.numeric(datesAxisH[13]))))              




mtext('B',adj=0,col=1,side=3,font=2,cex=0.8)



