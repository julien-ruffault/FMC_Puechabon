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
 
 
 
 
}  

FMCall85[is.na(FMCall85)==T] <- 65
FMCall45[is.na(FMCall45)==T] <- 65



FMCall85_2[is.na(FMCall85_2)==T] <- 5
FMCall45_2[is.na(FMCall45_2)==T] <- 5


# remove last year (2100)
 FMCall85 = FMCall85[-nrow(FMCall85),]
 FMCall45 = FMCall45[-nrow(FMCall45),]
 
 FMCall85_2 = FMCall85_2[-nrow(FMCall85_2),]
 FMCall45_2 = FMCall45_2[-nrow(FMCall45_2),]
 
 
 
 plot(FMCall85$year,rowMeans(FMCall85[,-1]),'l',col=1,lwd=2,ylim=c(10,100))
 for (i in 2:14){
   lines(FMCall85$year, FMCall85[,i],col=i,lwd=0.3)
 }
 lines(FMCall85$year,rowMeans(FMCall85[,-1]),'l',col=1,lwd=2,ylim=c(0,70))
 
 lines(FMCall45$year,rowMeans(FMCall45[,-1]),'l',col=1,lwd=2,ylim=c(0,70))
 
 
 plot(FMCall45$year,rowMeans(FMCall45[,-1]),'l',col=1,lwd=2,ylim=c(10,70))
 for (i in 2:14){
   lines(FMCall45$year, FMCall45[,i],col=i,lwd=0.3)
 }
 lines(FMCall45$year,rowMeans(FMCall45[,-1]),'l',col=1,lwd=2,ylim=c(0,70))
 
 
 
 
 plot(FMCall85_2$year,rowMeans(FMCall85_2[,-1]),'l',col=1,lwd=2,ylim=c(0,130))
 for (i in 2:14){
    lines(FMCall85_2$year, FMCall85_2[,i],col=i,lwd=0.3)
 }
 lines(FMCall85_2$year,rowMeans(FMCall85_2[,-1]),'l',col=1,lwd=2,ylim=c(0,70))
 
 lines(FMCall45_2$year,rowMeans(FMCall45_2[,-1]),'l',col=2,lwd=2,ylim=c(0,70))
 
 
 
 
 #plot(FMCall45)
 
 



 yall85 = list(FMCall85,FMCall85_2)
 yall45 = list(FMCall45,FMCall45_2)
 
# 
#  plot(yall85[[4]][,1],rowMeans(yall85[[4]][,2:14]),'l',col=1,lwd=2,ylim=c(0,1))
#  for (i in 2:14){
#    lines(yall85[[4]][,1], yall85[[4]][,i],col=i,lwd=0.3)
#  }
#  lines(FMCall85$year,rowMeans(FMCall85[,-1]),'l',col=1,lwd=2,ylim=c(0,70))
#  
#  

 # rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
 # gc() #free up memrory and report the memory usage.
 # 
 # #  UNCERTAINTY PARTITION ON DECADAL RUNNING MEAN
 ##t=ts$ts[[isce]][[climSim]]$t2 # years
 ##nvar is number of variables
 ##nsc is number of scenario
 ##nclimsim is number of clim sim
 
 library(caTools)
 
 t = FMCall45$year
 nvar = 2 #nvar =2 si ajout des proportions 
 nsc = 2 #nombre de scenar
 nclimSim=13
 
 it1=which(t==2010);it2=which(t==2090)
 source(paste0(mainDir,"/Figure_7_projections/myfilter.R"))
 

 # computation of the different term in uncertainty budget
 ymean = array(0,c(length(t),nvar,nsc));
 firesd = array(0,c(length(t),nvar,nsc)); # NB no firesd here, should remain equals to zero all the time
 climatesd = array(0,c(length(t),nvar,nsc));
 modelsd = array(0,c(length(t),nvar,nsc))
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
         
         ## BACK TO NORMAL SCRIPT
         ymean[,ivar,isce]=ymean[,ivar,isce]+1/nclimSim * y$trend
         trends[,i]=y$trend   # removing anomaly
         firesd[,ivar,isce]= 0 #firesd[,ivar,isce]+1/nclimSim * fa$firesd2
         climatesd[,ivar,isce]=climatesd[,ivar,isce]+1/nclimSim * y$climatsd
       }
       modelsd[,ivar,isce]=myfilt(apply(trends, c(1),sd),filter_30,iopt)
     }
   }
   y45=ymean[,,2]
   y85=ymean[,,1]
   scenariosd=abs(y85-y45)
   
   
   
   
   
   isce=2
   Fact=(firesd[,,isce]+climatesd[,,isce]+modelsd[,,isce])/sqrt(firesd[,,isce]^2+climatesd[,,isce]^2+modelsd[,,isce]^2)
   modelUnc45 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc45 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;modelClimateFireUnc45 = 1.654*(modelsd[,,isce]+firesd[,,isce]+climatesd[,,isce])/Fact
   y45mm = y45 + modelUnc45
   y45mmc = y45 + modelClimateUnc45
   y45mmcf = y45 + modelClimateFireUnc45
   
   isce=1
   Fact=(firesd[,,isce]+climatesd[,,isce]+modelsd[,,isce])/sqrt(firesd[,,isce]^2+climatesd[,,isce]^2+modelsd[,,isce]^2);
   modelUnc85 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc85 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;modelClimateFireUnc85 = 1.654*(modelsd[,,isce]+firesd[,,isce]+climatesd[,,isce])/Fact
   y85pm = y85 - modelUnc85
   y85pmc = y85 - modelClimateUnc85
   y85pmcf = y85 - modelClimateFireUnc85
   
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
     quartz(width=6.2,height=2.7)
     #plot.new()
     par(mfrow=c(1,2),mar=c(3,3,1,1.5))
     plot(t,y85[,ivar],ylim=c(0,max(y85pmcf[,ivar])+10),type='n',xlab=NA,ylab=NA,xaxs="i",yaxs="i",axes=F)
     abline(v=2005,lty=2,lwd=.5)
     # model
     polygon(c(rev(t),t),c(rev(y85[,ivar]),y85pm[,ivar])-offset,col=modcol,border=F)
     polygon(c(rev(t),t),c(rev(y45[,ivar]),y45mm[,ivar])-offset,col=modcol,border=F)
     lines(t,y85pm[,ivar],col=climcol,cex=2)
     lines(t,y45mm[,ivar],col=climcol,cex=2)
     # scenario
     polygon(c(rev(t),t),c(rev(y45[,ivar]),y85[,ivar])-offset,col=scecol,border=F) 
     lines(t,y85[,ivar]-offset,col='black',cex=2);lines(t,y45[,ivar]-offset,col='black',cex=2)
     # climat
     polygon(c(rev(t),t),c(rev(y85pm[,ivar]),y85pmc[,ivar])-offset,col=climcol,border=F);polygon(c(rev(t),t),c(rev(y45mm[,ivar]),y45mmc[,ivar])-offset,col=climcol,border=F)
     #lines(t,y85pmc[,ivar]-offset,col='black',cex=2);lines(t,y45mmc[,ivar]-offset,col='black',cex=2)
    
     axis(1,lwd.ticks=.5,lwd=0,cex.axis=0.72,tck=-0.03,mgp=c(2,.3,0))
     axis(2,las=2,lwd.ticks=.5,lwd=0,cex.axis=0.7,tck=-0.03,mgp=c(2,.6,0))
     box(lwd=.5)
     mtext(side=1,cex=0.72,'Year',line=1.2)
     mtext(side=2,cex=0.72,expression(CMC[min]*' (% dry mass)'),line=1.8)
     mtext(side=3,cex=0.8,'A',adj=0,font=2)
     
     
     
     isce=2
     Fact=(firesd[,,isce]+climatesd[,,isce]+modelsd[,,isce])/sqrt(firesd[,,isce]^2+climatesd[,,isce]^2+modelsd[,,isce]^2)
     modelUnc45 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc45 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;modelClimateFireUnc45 = 1.654*(modelsd[,,isce]+firesd[,,isce]+climatesd[,,isce])/Fact
     y45mm = y45 - modelUnc45
     y45mmc = y45 - modelClimateUnc45
     y45mmcf = y45 - modelClimateFireUnc45
     
     isce=1
     Fact=(firesd[,,isce]+climatesd[,,isce]+modelsd[,,isce])/sqrt(firesd[,,isce]^2+climatesd[,,isce]^2+modelsd[,,isce]^2);
     modelUnc85 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc85 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;modelClimateFireUnc85 = 1.654*(modelsd[,,isce]+firesd[,,isce]+climatesd[,,isce])/Fact
     y85pm = y85 + modelUnc85
     y85pmc = y85 + modelClimateUnc85
     y85pmcf = y85 + modelClimateFireUnc85
     
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
     plot(t,y85[,ivar],ylim=c(0,170),type='n',xlab=NA,ylab=NA,xaxs="i",yaxs="i",axes=F)
     abline(v=2005,lty=2,lwd=.5)
     # model
     polygon(c(rev(t),t),c(rev(y85[,ivar]),y85pm[,ivar])-offset,col=modcol,border=F)
     polygon(c(rev(t),t),c(rev(y45[,ivar]),y45mm[,ivar])-offset,col=modcol,border=F)
     lines(t,y85pm[,ivar],col=climcol,cex=2)
     lines(t,y45mm[,ivar],col=climcol,cex=2)
     # scenario
     polygon(c(rev(t),t),c(rev(y45[,ivar]),y85[,ivar])-offset,col=scecol,border=F) 
     lines(t,y85[,ivar]-offset,col='black',cex=2);lines(t,y45[,ivar]-offset,col='black',cex=2)
     # climat
     polygon(c(rev(t),t),c(rev(y85pm[,ivar]),y85pmc[,ivar])-offset,col=climcol,border=F);polygon(c(rev(t),t),c(rev(y45mm[,ivar]),y45mmc[,ivar])-offset,col=climcol,border=F)
     #lines(t,y85pmc[,ivar]-offset,col='black',cex=2);lines(t,y45mmc[,ivar]-offset,col='black',cex=2)
     
     axis(1,lwd.ticks=.5,lwd=0,cex.axis=0.72,tck=-0.03,mgp=c(2,.3,0))
     axis(2,las=2,lwd.ticks=.5,lwd=0,cex.axis=0.7,tck=-0.03,mgp=c(2,.6,0))
     box(lwd=.5)
     mtext(side=1,cex=0.72,'Year',line=1.2)
     mtext(side=2,cex=0.72,'FSL (days)',line=1.8)
     mtext(side=3,cex=0.8,'B',adj=0,font=2)
     
     #lines(t,y85pmcf[,ivar]-offset,col='black',cex=2);lines(t,y45mmcf[,ivar]-offset,col='black',cex=2)
     # if (ivar==1) {mtext(side=3,'(a)',adj=0,cex=1.2)}
     # if (ivar==6) {
     #   mtext(side=3,'(b)',adj=0,cex=1.2)
     #   mtext(side=3,'Decadal trends',cex=1.5)
     # }
     # if (ivar==9) {mtext(side=3,'(c)',adj=0,cex=1.2)}
     #   print("RCP85")
     #   print(c(ivar," y85= 2010",signif(y85[it1,ivar]-offset, 3),"2090=",signif(y85[it2,ivar]-offset, 3)))
     #   print(c(ivar," +/-= 2010",signif(y85pmcf[it1,ivar]-y85[it1,ivar], 3),"2090=",signif(y85pmcf[it2,ivar]-y85[it2,ivar], 3)))
     #   print(c(ivar," FU =",signif(y85pmcf[it1,ivar]-y85pmc[it1,ivar], 3),"2090=",signif(y85pmcf[it2,ivar]-y85pmc[it2,ivar], 3)))
     #   print(c(ivar," CIV =",signif(y85pmc[it1,ivar]-y85pm[it1,ivar], 3),"2090=",signif(y85pmc[it2,ivar]-y85pm[it2,ivar], 3)))
     #   print(c(ivar," CMU =",signif(y85pm[it1,ivar]-y85[it1,ivar], 3),"2090=",signif(y85pm[it2,ivar]-y85[it2,ivar], 3)))
     #   print("RCP45")
     #   print(c(ivar," y45= 2010",signif(y45[it1,ivar]-offset, 3),"2090=",signif(y45[it2,ivar]-offset, 3)))
     #   print(c(ivar,"+/-= 2010",signif(y45[it1,ivar]-y45mmcf[it1,ivar], 3),"2090=",signif(y45[it2,ivar]-y45mmcf[it2,ivar], 3)))
     #   print(c(ivar," FU =",signif(y45mmc[it1,ivar]-y45mmcf[it1,ivar], 3),"2090=",signif(y45mmc[it2,ivar]-y45mmcf[it2,ivar], 3)))
     #   print(c(ivar," CIV =",signif(y45mm[it1,ivar]-y45mmc[it1,ivar], 3),"2090=",signif(y45mm[it2,ivar]-y45mmc[it2,ivar], 3)))
     #   print(c(ivar," CMU =",signif(y45[it1,ivar]-y45mm[it1,ivar], 3),"2090=",signif(y45[it2,ivar]-y45mm[it2,ivar], 3)))
     # # 
     # # #lines(x,y,col='black',cex=2)
     # # #mtext(side=3,paste0('Uncertainty partition (decadal mean)'),cex=1.5)
     # mtext(side=2,ts$varlab[ivar],cex=1.5,line=2)
     # #axis(1,at=x,labels=varlab,las=1);axis(2,las=1) #axis(1,cex.axis=1,tck=0.02,at=x,labels=c('N1','N10','N100','N500','N1000','BA'),line=-1.2,lwd=0,lwd.ticks =0.5,tck=0.035) #axis(2,cex.axis=1,tck=0,at=seq(0,ymax,20),labels=seq(0,ymax,20),line=-1.2,lwd=0)
     # legend("topleft", bty="n",legend=c("Scenario","Clim. model","Clim. internal"),lwd=2,col=c(scecol,modcol,climcol,firecol))
     # 
     # ierr=50;ndg=3;dpi=300
     # ierr=it2
     # uncertainty = signif((y85pmcf[ierr,ivar]-y85[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
     # fireuncertainty = signif((y85pmcf[ierr,ivar]-y85pmc[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
     # climateuncertainty = signif((y85pmc[ierr,ivar]-y85pm[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
     # cmuncertainty = signif((y85pm[ierr,ivar]-y85[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
     # climatchange=signif((y85[it2,ivar]-y85[it1,ivar])/y85[it1,ivar]*100,ndg)
     # ymax=max(y85pmcf[,ivar])
     # text(x=2012,y=ymax*0.16-offset,paste0('RCP 8.5 Climate Change: +',climatchange,'%'),cex=1,adj=0, col='red')
     # text(x=2012,y=ymax*0.12-offset,paste0('Uncertainty: ',uncertainty,'% (CIV: ',climateuncertainty,'%, FU:',fireuncertainty,'%, CMU:',cmuncertainty,')'),cex=0.8,adj=0, col=Colors_line[1])
     # 
     # uncertainty = -signif((y45mmcf[ierr,ivar]-y45[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
     # fireuncertainty = - signif((y45mmcf[ierr,ivar]-y45mmc[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
     # climateuncertainty = - signif((y45mmc[ierr,ivar]-y45mm[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
     # cmuncertainty = - signif((y45mm[ierr,ivar]-y45[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
     # climatchange=signif((y45[it2,ivar]-y45[it1,ivar])/y45[it1,ivar]*100,ndg)
     # ymax=max(y85pmcf[,ivar])
     # text(x=2012,y=ymax*0.06-offset,paste0('RCP 4.5 Climate Change: +',climatchange,'%'),cex=1,adj=0, col='blue')
     # text(x=2012,y=ymax*0.02-offset,paste0('Uncertainty: ',uncertainty,'% (CIV: ',climateuncertainty,'%, FU:',fireuncertainty,'%, CMU:',cmuncertainty,')'),cex=0.8,adj=0, col=Colors_line[2])
     # box(lwd=0.5)
     # dev.off()
     # 
     # 

     