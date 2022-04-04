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



FMCall45 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))
FMCall85 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))

FMCall45$year=1985:2100
FMCall85$year=1985:2100



FMCall45_2 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))
FMCall85_2 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))

FMCall45_2$year=1985:2100
FMCall85_2$year=1985:2100



FMCall45_3 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))
FMCall85_3 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))

FMCall45_3$year=1985:2100
FMCall85_3$year=1985:2100


FMCall45_4 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))
FMCall85_4 = setNames(data.frame(matrix(nrow=length(1985:2100),ncol=length(MODELS_NAME)+1)),c('year',MODELS_NAME))

FMCall45_4$year=1985:2100
FMCall85_4$year=1985:2100

for (MOD in 1:length(MODELS_NAME))
{
   # filename = paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_rcp85.csv')
   # DATA      = read.csv(filename,header=T, dec='.', sep="")
   io_histo = read.csv(paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_histo.csv'),
                       header=T, dec='.', sep="")
   io_rcp85 = read.csv(paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_rcp85.csv'),
                       header=T, dec='.', sep="")
   
   io_rcp45 = read.csv(paste0(mainDir,'/Figure_7_projections/FMC_projected/',MODELS_NAME[MOD],'/FMC_rcp45.csv'),
                       header=T, dec='.', sep="")
   
   
   FMCall85[1:21,(MOD+1)] = io_histo$yearly_FMCCanopy_min
   FMCall85[22:116,(MOD+1)] = io_rcp85$yearly_FMCCanopy_min
   
   FMCall45[1:21,(MOD+1)] = io_histo$yearly_FMCCanopy_min
   FMCall45[22:116,(MOD+1)] = io_rcp45$yearly_FMCCanopy_min
   
   
   FMCall85_2[1:21,(MOD+1)] = io_histo$yearly_nbDayLFMC_67
   FMCall85_2[22:116,(MOD+1)] = io_rcp85$yearly_nbDayLFMC_67
   
   FMCall45_2[1:21,(MOD+1)] = io_histo$yearly_nbDayLFMC_67
   FMCall45_2[22:116,(MOD+1)] = io_rcp45$yearly_nbDayLFMC_67
   
   
   FMCall85_3[1:21,(MOD+1)] = io_histo$yearly_PLC_Leaf_max
   FMCall85_3[22:116,(MOD+1)] = io_rcp85$yearly_PLC_Leaf_max
   
   FMCall45_3[1:21,(MOD+1)] = io_histo$yearly_PLC_Leaf_max
   FMCall45_3[22:116,(MOD+1)] = io_rcp45$yearly_PLC_Leaf_max
   
   FMCall85_4[1:21,(MOD+1)]   = io_histo$yearly_LFMC_min
   FMCall85_4[22:116,(MOD+1)] = io_rcp85$yearly_LFMC_min
   
   FMCall45_4[1:21,(MOD+1)]   = io_histo$yearly_LFMC_min
   FMCall45_4[22:116,(MOD+1)] = io_rcp45$yearly_LFMC_min
   
   
   
}  

FMCall85[is.na(FMCall85)==T] <- 65
FMCall45[is.na(FMCall45)==T] <- 65



FMCall85_2[is.na(FMCall85_2)==T] <- 5
FMCall45_2[is.na(FMCall45_2)==T] <- 5


FMCall85_3[is.na(FMCall85_3)==T] <- 0
FMCall45_3[is.na(FMCall45_3)==T] <- 0

FMCall85_4[is.na(FMCall85_4)==T] <- 70
FMCall45_4[is.na(FMCall45_4)==T] <- 70



# remove last year (2100)
# remove last year (2100)
FMCall85 = FMCall85[-nrow(FMCall85),]
FMCall45 = FMCall45[-nrow(FMCall45),]

FMCall85_2 = FMCall85_2[-nrow(FMCall85_2),]
FMCall45_2 = FMCall45_2[-nrow(FMCall45_2),]

FMCall85_3 = FMCall85_3[-nrow(FMCall85_3),]
FMCall45_3 = FMCall45_3[-nrow(FMCall45_3),]

FMCall85_4 = FMCall85_4[-nrow(FMCall85_4),]
FMCall45_4 = FMCall45_4[-nrow(FMCall45_4),]


yall85 = list(FMCall85,FMCall85_2,FMCall85_3,FMCall85_4)
yall45 = list(FMCall45,FMCall45_2,FMCall45_3,FMCall45_4)





# #  UNCERTAINTY PARTITION ON DECADAL RUNNING MEAN
##t=ts$ts[[isce]][[climSim]]$t2 # years
##nvar is number of variables
##nsc is number of scenario
##nclimsim is number of clim sim

library(caTools)

t = FMCall45$year
nvar = 4 #nvar =2 si ajout des proportions 
nsc = 2 #nombre de scenar
nclimSim=13

it1=which(t==2010);it2=which(t==2090)
source(paste0(mainDir,"/Figure_7_projections/myfilter.R"))


# computation of the different term in uncertainty budget
ymean = array(0,c(length(t),nvar,nsc));
#firesd = array(0,c(length(t),nvar,nsc)); # NB no firesd here, should remain equals to zero all the time
climatesd = array(0,c(length(t),nvar,nsc));
climateQ05= array(0,c(length(t),nvar,nsc));
climateQ95= array(0,c(length(t),nvar,nsc));
modelsd = array(0,c(length(t),nvar,nsc))
modelQ05 = array(0,c(length(t),nvar,nsc))
modelQ95 = array(0,c(length(t),nvar,nsc))
for (isce in c(1:nsc)) {
   for (ivar in c(1:nvar)) { 
      trends = array(0,c(length(t),nclimSim))
      i=0;
      for (climSim in 2:14) {
         i=i+1;
         y=NULL
         if (isce==1)
         {y$values = yall85[[ivar]][,climSim]}
         
         if (isce==2)
         {y$values = yall45[[ivar]][,climSim]}
         
         ##y = ts$ts[[isce]][[climSim]]$fa[[ivar]]$tenth$runningMean # this contains the series,  its decadal trend and sd,its running mean
         ## RECOMPUTED HERE LOCALLY
         ## decadal Running Mean with padding
         k=10;k1=floor((k-1)/2)  
         y$runningMean=0*y$values
         sz=k-1;ytmp= paddingDouble(y$values,"slope0_cst",sz,sz) # we add k value at the beginning and the end
         for (j in c(1:length(y$values))) {
            print(j)
            tmp = ytmp[(j+k-k1):(j+2*k-k1-1)] # running dataset of interest
            #if ivar==1 -->mean else if ivar==2 > mean(tmp<thres)
            y$runningMean[j]=mean(tmp) # remplacer par la proportion dans le cas on s'interesse à des seuils (min_LFMC<critical_FMCthres)
         }
         ## 30 year trend
         y$trend = myfilt(y$runningMean,filter_30,iopt)
         ## climateSD
         y$climatsd=runsd(y$runningMean,30,center=y$trend)
         y$climatQ05=runquantile(y$runningMean,30,probs=0.05)
         y$climatQ95=runquantile(y$runningMean,30,probs=0.95)
         
         ## BACK TO NORMAL SCRIPT
         ymean[,ivar,isce]=ymean[,ivar,isce]+1/nclimSim * y$trend
         trends[,i]=y$trend   # removing anomaly
         #firesd[,ivar,isce]= 0 #firesd[,ivar,isce]+1/nclimSim * fa$firesd2
         climatesd[,ivar,isce]=climatesd[,ivar,isce]+1/nclimSim * y$climatsd
         climateQ05[,ivar,isce]=climateQ05[,ivar,isce]+1/nclimSim * y$climatQ05
         climateQ95[,ivar,isce]=climateQ95[,ivar,isce]+1/nclimSim * y$climatQ95
      }
      modelsd[,ivar,isce]=myfilt(apply(trends, c(1),sd),filter_30,iopt)
      modelQ05[,ivar,isce]=myfilt(apply(trends, c(1),quantile,0.05),filter_30,iopt)
      modelQ95[,ivar,isce]=myfilt(apply(trends, c(1),quantile,0.95),filter_30,iopt)
   }
}
y45=ymean[,,2]
y85=ymean[,,1]
scenariosd=abs(y85-y45)

### FIGURE ####


quartz(width=6.2,height=5)
#plot.new()
par(mfrow=c(2,2),mar=c(3,3,1,1.5))



isce=2
Fact=(climatesd[,,isce]+modelsd[,,isce])/sqrt(climatesd[,,isce]^2+modelsd[,,isce]^2)
modelUnc45 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc45 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact
y45mm = y45 + modelUnc45
y45mmc = y45 + modelClimateUnc45

# FIGURE WITH ASYMETRIC UNCERTAINTIES
climates95=climateQ95[,,isce]-y45
models95=modelQ95[,,isce]-y45
Fact95=(climates95+models95)/sqrt(climates95^2+models95^2)
modelUnc45_95 = models95/Fact95;modelClimateUnc45_95 = (models95+climates95)/Fact95
y45mm = y45 + modelUnc45_95
y45mmc = y45 + modelClimateUnc45_95



isce=1
Fact=(climatesd[,,isce]+modelsd[,,isce])/sqrt(climatesd[,,isce]^2+modelsd[,,isce]^2);
modelUnc85 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc85 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;
y85pm = y85 - modelUnc85
y85pmc = y85 - modelClimateUnc85

# FIGURE WITH ASYMETRIC UNCERTAINTIES
climates05=y85-climateQ05[,,isce]
models05=y85-modelQ05[,,isce]
Fact05=(climates05+models05)/sqrt(climates05^2+models05^2)
modelUnc85_05 = models05/Fact05;modelClimateUnc85_05 = (models05+climates05)/Fact05
y85pm = y85 - modelUnc85_05
y85pmc = y85 - modelClimateUnc85_05


offset=0
resval = 100;widthv = 120;heightv = 120;
scecol=rgb(132,186,91,alpha=150,maxColorValue=256)
climcol=rgb(114,147,203,alpha=150,maxColorValue=256)
modcol =rgb(128,133,133,alpha=150,maxColorValue=256)
firecol='red'
#for (ivar in c(1,2,3,4)) { #ivar=9
ivar=1 
offset=0
#png(file=paste0(PATH,'figs/model_means2/',ts$varlab[ivar],"_uncertainty",anom,".png"),res=resval,width = widthv,height = heightv, units="mm")
#quartz(width=6.8,height=6.0)
#plot.new()
#par(new = "TRUE" ,plt = c(0.15,0.95,0.09,0.95),cex.axis = 1)

plot(t,y85[,ivar],ylim=c(0,max(y85pmc[,ivar])+10),type='n',xlab=NA,ylab=NA,xaxs="i",yaxs="i",axes=F)
# model
polygon(c(rev(t),t),c(rev(y85[,ivar]),y85pm[,ivar])-offset,col=modcol,border=F)
polygon(c(rev(t),t),c(rev(y45[,ivar]),y45mm[,ivar])-offset,col=modcol,border=F)
lines(t,y85pm[,ivar],col=climcol,cex=2)
lines(t,y45mm[,ivar],col=climcol,cex=2)
# scenario
polygon(c(rev(t),t),c(rev(y45[,ivar]),y85[,ivar])-offset,col=scecol,border=F) 
lines(t,y85[,ivar]-offset,col='black',cex=2)
lines(t,y45[,ivar]-offset,col='black',cex=2,lty=2)
# climat
y85pmc[,ivar]=myfilt(y85pmc[,ivar],filter_30,iopt)
y45mmc[,ivar]=myfilt(y45mmc[,ivar],filter_30,iopt)
polygon(c(rev(t),t),c(rev(y85pm[,ivar]),y85pmc[,ivar])-offset,col=climcol,border=F);polygon(c(rev(t),t),c(rev(y45mm[,ivar]),y45mmc[,ivar])-offset,col=climcol,border=F)
#lines(t,y85pmc[,ivar]-offset,col='black',cex=2);lines(t,y45mmc[,ivar]-offset,col='black',cex=2)


axis(1,lwd.ticks=.5,lwd=0,cex.axis=0.72,tck=-0.03,mgp=c(2,.3,0))
axis(2,las=2,lwd.ticks=.5,lwd=0,cex.axis=0.7,tck=-0.03,mgp=c(2,.6,0))
box(lwd=.5)
mtext(side=1,cex=0.72,'Year',line=1.2)
mtext(side=2,cex=0.72,expression(CFMC[min]*' (% dry mass)'),line=1.8)
mtext(side=3,cex=0.8,'A',adj=0,font=2)





# FSLmin 

isce=2
Fact=(climatesd[,,isce]+modelsd[,,isce])/sqrt(climatesd[,,isce]^2+modelsd[,,isce]^2)
modelUnc45 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc45 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact
y45mm = y45 - modelUnc45
y45mmc = y45 - modelClimateUnc45

climates05=y45-climateQ05[,,isce]
models05=y45-modelQ05[,,isce]
Fact05=(climates05+models05)/sqrt(climates05^2+models05^2)
modelUnc45_05 = models05/Fact05;modelClimateUnc45_05 = (models05+climates05)/Fact05
y45mm = y45 - modelUnc45_05
y45mmc = y45 - modelClimateUnc45_05 

isce=1
Fact=(climatesd[,,isce]+modelsd[,,isce])/sqrt(climatesd[,,isce]^2+modelsd[,,isce]^2);
modelUnc85 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc85 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;
y85pm = y85 + modelUnc85
y85pmc = y85 + modelClimateUnc85

climates95=climateQ95[,,isce]-y85
models95=modelQ95[,,isce]-y85
Fact95=(climates95+models95)/sqrt(climates95^2+models95^2)
modelUnc85_95 = models95/Fact95;modelClimateUnc85_95 = (models95+climates95)/Fact95
y85pm = y85 + modelUnc85_95
y85pmc = y85 + modelClimateUnc85_95 #




offset=0
resval = 100;widthv = 120;heightv = 120;
scecol=rgb(132,186,91,alpha=150,maxColorValue=256)
climcol=rgb(114,147,203,alpha=150,maxColorValue=256)
modcol =rgb(128,133,133,alpha=150,maxColorValue=256)
firecol='red'
#for (ivar in c(1,2,3,4)) { #ivar=9
ivar=1 
offset=0



ivar=2 
offset=0
#png(file=paste0(PATH,'figs/model_means2/',ts$varlab[ivar],"_uncertainty",anom,".png"),res=resval,width = widthv,height = heightv, units="mm")
#quartz(width=6.8,height=6.0)
#plot.new()
#par(new = "TRUE" ,plt = c(0.15,0.95,0.09,0.95),cex.axis = 1)

plot(t,y85[,ivar],ylim=c(0,180),type='n',xlab=NA,ylab=NA,xaxs="i",yaxs="i",axes=F)
# model
polygon(c(rev(t),t),c(rev(y85[,ivar]),y85pm[,ivar])-offset,col=modcol,border=F)
polygon(c(rev(t),t),c(rev(y45[,ivar]),y45mm[,ivar])-offset,col=modcol,border=F)
lines(t,y85pm[,ivar],col=climcol,cex=2)
lines(t,y45mm[,ivar],col=climcol,cex=2)
# scenario
polygon(c(rev(t),t),c(rev(y45[,ivar]),y85[,ivar])-offset,col=scecol,border=F) 
lines(t,y85[,ivar]-offset,col='black',cex=2)
lines(t,y45[,ivar]-offset,col='black',cex=2,lty=2)
# climat
y85pmc[,ivar]=myfilt(y85pmc[,ivar],filter_30,iopt)
y45mmc[,ivar]=myfilt(y45mmc[,ivar],filter_30,iopt)
polygon(c(rev(t),t),c(rev(y85pm[,ivar]),y85pmc[,ivar])-offset,col=climcol,border=F)
polygon(c(rev(t),t),c(rev(y45mm[,ivar]),y45mmc[,ivar])-offset,col=climcol,border=F)
#lines(t,y85pmc[,ivar]-offset,col='black',cex=2);lines(t,y45mmc[,ivar]-offset,col='black',cex=2)


axis(1,lwd.ticks=.5,lwd=0,cex.axis=0.72,tck=-0.03,mgp=c(2,.3,0))
axis(2,las=2,lwd.ticks=.5,lwd=0,cex.axis=0.7,tck=-0.03,mgp=c(2,.6,0))
box(lwd=.5)
mtext(side=1,cex=0.72,'Year',line=1.2)
mtext(side=2,cex=0.72,'FSL (days)',line=1.8)
mtext(side=3,cex=0.8,'B',adj=0,font=2)




ivar=3 
offset=0

plot(t,y85[,ivar],ylim=c(0,100),type='n',xlab=NA,ylab=NA,xaxs="i",yaxs="i",axes=F)
# model
polygon(c(rev(t),t),c(rev(y85[,ivar]),y85pm[,ivar])-offset,col=modcol,border=F)
polygon(c(rev(t),t),c(rev(y45[,ivar]),y45mm[,ivar])-offset,col=modcol,border=F)
lines(t,y85pm[,ivar],col=climcol,cex=2)
lines(t,y45mm[,ivar],col=climcol,cex=2)
# scenario
polygon(c(rev(t),t),c(rev(y45[,ivar]),y85[,ivar])-offset,col=scecol,border=F) 
lines(t,y85[,ivar]-offset,col='black',cex=2)
lines(t,y45[,ivar]-offset,col='black',cex=2,lty=2)
# climat
y85pmc[,ivar]=myfilt(y85pmc[,ivar],filter_30,iopt)
y45mmc[,ivar]=myfilt(y45mmc[,ivar],filter_30,iopt)
polygon(c(rev(t),t),c(rev(y85pm[,ivar]),y85pmc[,ivar])-offset,col=climcol,border=F)
polygon(c(rev(t),t),c(rev(y45mm[,ivar]),y45mmc[,ivar])-offset,col=climcol,border=F)
#lines(t,y85pmc[,ivar]-offset,col='black',cex=2);lines(t,y45mmc[,ivar]-offset,col='black',cex=2)



axis(1,lwd.ticks=.5,lwd=0,cex.axis=0.72,tck=-0.03,mgp=c(2,.3,0))
axis(2,las=2,lwd.ticks=.5,lwd=0,cex.axis=0.7,tck=-0.03,mgp=c(2,.6,0))
box(lwd=.5)
mtext(side=1,cex=0.72,'Year',line=1.2)
mtext(side=2,cex=0.72,'PLC (%)',line=1.8)
mtext(side=3,cex=0.8,'C',adj=0,font=2)




plot(1:10,1:10,type='n',axes=F,xlab='',ylab="")


legend("left", bty="n",cex=0.9,
       legend=c('Decadal mean RCP45',"Decadal mean RCP85","Scenario uncertainty","Model uncertainty","Internal uncertainty"),
       lwd=c(1.2,1.2,0,0,0),
       lty=c(2,1,NA,NA,NA),
       pt.cex=1.3,
       pt.bg= c(NA,NA,scecol,modcol,climcol),
       col=c(1,1,NA,NA,NA),
       pch=c(NA,NA,22,22,22))





