# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
# gc() #free up memrory and report the memory usage.
# 
# #  UNCERTAINTY PARTITION ON DECADAL RUNNING MEAN
##t=ts$ts[[isce]][[climSim]]$t2 # years
##nvar is number of variables
##nsc is number of scenario
##nclimsim is number of clim sim

library(caTools)

t = FMCall$year
nvar = 2 #nvar =2 si ajout des proportions 
nsc = 2 #nombre de scenar
nclimSim=13

it1=which(t==2010);it2=which(t==2090)
source(paste0(PATH,"myfilter.R"))
# for anomaly we only represent the increase
for (anom in c("")) {#anom="";
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
        y$values = FMCall[,climSim]
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
        trends[,i]=y$trend - (anom=="_anomaly") * mean(y$trend[1])  # removing anomaly
        firesd[,ivar,isce]= 0 #firesd[,ivar,isce]+1/nclimSim * fa$firesd2
        climatesd[,ivar,isce]=climatesd[,ivar,isce]+1/nclimSim * y$climatsd
      }
      modelsd[,ivar,isce]=myfilt(apply(trends, c(1),sd),filter_30,iopt)
    }
  }
  y45=ymean[,,2];y85=ymean[,,1];scenariosd=abs(y85-y45)
  isce=2;Fact=(firesd[,,isce]+climatesd[,,isce]+modelsd[,,isce])/sqrt(firesd[,,isce]^2+climatesd[,,isce]^2+modelsd[,,isce]^2);
  modelUnc45 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc45 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;modelClimateFireUnc45 = 1.654*(modelsd[,,isce]+firesd[,,isce]+climatesd[,,isce])/Fact
  y45mm = y45 - modelUnc45;y45mmc = y45 - modelClimateUnc45;y45mmcf = y45 - modelClimateFireUnc45;
  isce=1;Fact=(firesd[,,isce]+climatesd[,,isce]+modelsd[,,isce])/sqrt(firesd[,,isce]^2+climatesd[,,isce]^2+modelsd[,,isce]^2);
  modelUnc85 = 1.654*modelsd[,,isce]/Fact;modelClimateUnc85 = 1.654*(modelsd[,,isce]+climatesd[,,isce])/Fact;modelClimateFireUnc85 = 1.654*(modelsd[,,isce]+firesd[,,isce]+climatesd[,,isce])/Fact
  y85pm = y85 + modelUnc85;y85pmc = y85 + modelClimateUnc85;y85pmcf = y85 + modelClimateFireUnc85;
  
  resval = 100;widthv = 120;heightv = 120;
  scecol='green';modcol='blue';
  climcol ='yellow'
  firecol='red'
  for (ivar in c(1)) { #ivar=9
    if (anom=="_anomaly") {
      offset = 0.5*(ymean[1,ivar,1]+ymean[1,ivar,2])  
    } else {offset=0}
    #png(file=paste0(PATH,'figs/model_means2/',ts$varlab[ivar],"_uncertainty",anom,".png"),res=resval,width = widthv,height = heightv, units="mm")
    #quartz(width=6.8,height=6.0)
    #plot.new()
    par(new = "TRUE" ,plt = c(0.15,0.95,0.09,0.95),cex.axis = 1)
    plot(t,y85[,ivar],ylim=c(0,max(y85pmcf[,ivar]))-offset,type='n',xlab=NA,ylab=NA,xaxs="i",yaxs="i")
    # model
    polygon(c(rev(t),t),c(rev(y85[,ivar]),y85pm[,ivar])-offset,col=modcol,border=F);polygon(c(rev(t),t),c(rev(y45[,ivar]),y45mm[,ivar])-offset,col=modcol,border=F)
    lines(t,y85pm[,ivar]-offset,col='black',cex=2);lines(t,y45mm[,ivar]-offset,col='black',cex=2)
    # scenario
    polygon(c(rev(t),t),c(rev(y45[,ivar]),y85[,ivar])-offset,col='green',border=F) 
    lines(t,y85[,ivar]-offset,col='black',cex=2);lines(t,y45[,ivar]-offset,col='black',cex=2)
    # climat
    polygon(c(rev(t),t),c(rev(y85pm[,ivar]),y85pmc[,ivar])-offset,col=climcol,border=F);polygon(c(rev(t),t),c(rev(y45mm[,ivar]),y45mmc[,ivar])-offset,col=climcol,border=F)
    lines(t,y85pmc[,ivar]-offset,col='black',cex=2);lines(t,y45mmc[,ivar]-offset,col='black',cex=2)
    # fire
    polygon(c(rev(t),t),c(rev(y85pmc[,ivar]),y85pmcf[,ivar])-offset,col=firecol,border=F);polygon(c(rev(t),t),c(rev(y45mmc[,ivar]),y45mmcf[,ivar])-offset,col=firecol,border=F)
    lines(t,y85pmcf[,ivar]-offset,col='black',cex=2);lines(t,y45mmcf[,ivar]-offset,col='black',cex=2)
    if (ivar==1) {mtext(side=3,'(a)',adj=0,cex=1.2)}
    if (ivar==6) {
      mtext(side=3,'(b)',adj=0,cex=1.2)
      mtext(side=3,'Decadal trends',cex=1.5)
    }
    if (ivar==9) {mtext(side=3,'(c)',adj=0,cex=1.2)}
    if (anom!="_anomaly") {
      print("RCP85")
      print(c(ivar," y85= 2010",signif(y85[it1,ivar]-offset, 3),"2090=",signif(y85[it2,ivar]-offset, 3)))
      print(c(ivar," +/-= 2010",signif(y85pmcf[it1,ivar]-y85[it1,ivar], 3),"2090=",signif(y85pmcf[it2,ivar]-y85[it2,ivar], 3)))
      print(c(ivar," FU =",signif(y85pmcf[it1,ivar]-y85pmc[it1,ivar], 3),"2090=",signif(y85pmcf[it2,ivar]-y85pmc[it2,ivar], 3)))
      print(c(ivar," CIV =",signif(y85pmc[it1,ivar]-y85pm[it1,ivar], 3),"2090=",signif(y85pmc[it2,ivar]-y85pm[it2,ivar], 3)))
      print(c(ivar," CMU =",signif(y85pm[it1,ivar]-y85[it1,ivar], 3),"2090=",signif(y85pm[it2,ivar]-y85[it2,ivar], 3)))
      print("RCP45")
      print(c(ivar," y45= 2010",signif(y45[it1,ivar]-offset, 3),"2090=",signif(y45[it2,ivar]-offset, 3)))
      print(c(ivar,"+/-= 2010",signif(y45[it1,ivar]-y45mmcf[it1,ivar], 3),"2090=",signif(y45[it2,ivar]-y45mmcf[it2,ivar], 3)))
      print(c(ivar," FU =",signif(y45mmc[it1,ivar]-y45mmcf[it1,ivar], 3),"2090=",signif(y45mmc[it2,ivar]-y45mmcf[it2,ivar], 3)))
      print(c(ivar," CIV =",signif(y45mm[it1,ivar]-y45mmc[it1,ivar], 3),"2090=",signif(y45mm[it2,ivar]-y45mmc[it2,ivar], 3)))
      print(c(ivar," CMU =",signif(y45[it1,ivar]-y45mm[it1,ivar], 3),"2090=",signif(y45[it2,ivar]-y45mm[it2,ivar], 3)))
    } else {
      print("ANOMALY")
      print(c(ivar," ",anom," Delta_y85=",signif(y85[it2,ivar]-offset, 3),"+/_",signif(y85pmcf[it2,ivar]-y85[it2,ivar], 3)))
      print(c(ivar," ",anom," FU=",signif(y85pmcf[it2,ivar]-y85pmc[it2,ivar], 3)," CIV=",signif(y85pmc[it2,ivar]-y85pm[it2,ivar], 3)," CMU=",signif(y85pm[it2,ivar]-y85[it2,ivar], 3)))
      print(c(ivar," ",anom," Delta_y45=",signif(y45[it2,ivar]-offset, 3),"+/_",signif(y45[it2,ivar]-y45mmcf[it2,ivar], 3)))
      print(c(ivar," ",anom," FU=",signif(y45mmc[it2,ivar]-y45mmcf[it2,ivar], 3)," CIV=",signif(y45mm[it2,ivar]-y45mmc[it2,ivar], 3)," CMU=",signif(y45mm[it2,ivar]-y45[it2,ivar], 3)))
    }
    #lines(x,y,col='black',cex=2)
    #mtext(side=3,paste0('Uncertainty partition (decadal mean)'),cex=1.5)
    mtext(side=2,ts$varlab[ivar],cex=1.5,line=2)
    #axis(1,at=x,labels=varlab,las=1);axis(2,las=1) #axis(1,cex.axis=1,tck=0.02,at=x,labels=c('N1','N10','N100','N500','N1000','BA'),line=-1.2,lwd=0,lwd.ticks =0.5,tck=0.035) #axis(2,cex.axis=1,tck=0,at=seq(0,ymax,20),labels=seq(0,ymax,20),line=-1.2,lwd=0)
    legend("topleft", bty="n",legend=c("Scenario","Clim. model","Clim. internal","Fire"),lwd=2,col=c(scecol,modcol,climcol,firecol))
    
    ierr=50;ndg=3;dpi=300
    ierr=it2
    uncertainty = signif((y85pmcf[ierr,ivar]-y85[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
    fireuncertainty = signif((y85pmcf[ierr,ivar]-y85pmc[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
    climateuncertainty = signif((y85pmc[ierr,ivar]-y85pm[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
    cmuncertainty = signif((y85pm[ierr,ivar]-y85[ierr,ivar])/y85[ierr,ivar]*100,ndg) 
    climatchange=signif((y85[it2,ivar]-y85[it1,ivar])/y85[it1,ivar]*100,ndg)
    ymax=max(y85pmcf[,ivar])
    text(x=2012,y=ymax*0.16-offset,paste0('RCP 8.5 Climate Change: +',climatchange,'%'),cex=1,adj=0, col='red')
    text(x=2012,y=ymax*0.12-offset,paste0('Uncertainty: ',uncertainty,'% (CIV: ',climateuncertainty,'%, FU:',fireuncertainty,'%, CMU:',cmuncertainty,')'),cex=0.8,adj=0, col=Colors_line[1])
    
    uncertainty = -signif((y45mmcf[ierr,ivar]-y45[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
    fireuncertainty = - signif((y45mmcf[ierr,ivar]-y45mmc[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
    climateuncertainty = - signif((y45mmc[ierr,ivar]-y45mm[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
    cmuncertainty = - signif((y45mm[ierr,ivar]-y45[ierr,ivar])/y45[ierr,ivar]*100,ndg) 
    climatchange=signif((y45[it2,ivar]-y45[it1,ivar])/y45[it1,ivar]*100,ndg)
    ymax=max(y85pmcf[,ivar])
    text(x=2012,y=ymax*0.06-offset,paste0('RCP 4.5 Climate Change: +',climatchange,'%'),cex=1,adj=0, col='blue')
    text(x=2012,y=ymax*0.02-offset,paste0('Uncertainty: ',uncertainty,'% (CIV: ',climateuncertainty,'%, FU:',fireuncertainty,'%, CMU:',cmuncertainty,')'),cex=0.8,adj=0, col=Colors_line[2])
    box(lwd=0.5)
    dev.off()
    
  }
}
