# Figure sensitivity analysis FMC puechabon 
# Simplified/ ith groups 
# Created : 11/05/2022
# Author : Julien Ruffault (julien.ruff@gmail.com)


# clean environment
rm(list=ls(all=TRUE));gc()
library(sensobol)

multistack.bar <- function(x=list(x), betweenspace = 2, withinspace=0, ...){
  # note that number of rows in each component matrix of the list
  # should be equal
  # if you have missing value use "NA", to make it complete 
  # number of column can differ
  mylist <- x
  space = list()
  space[[1]] <- c(rep(withinspace,ncol(mylist[[1]] )),betweenspace ) 
  
  for ( i in 2:length(mylist)){
    if(i == length(mylist)){
      space[[i]] <- c(rep(withinspace,ncol(mylist[[i]] )-1)) 
    } else{
      space[[i]] <- c(rep(withinspace,ncol(mylist[[i]] )-1),betweenspace ) 
    }
  }
  un.space <- c(unlist(space))
  newdata <- do.call("cbind", mylist)
  barplot(newdata, space= un.space, ...)
}




IND1=NULL



directoryToRefSimu = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/sensitivity_FMC_7/"
mainDirOutpout =directoryToRefSimu
Out_dir <- file.path(mainDirOutpout)


PARAMS =   io =read.csv(paste0(Out_dir,'/PARAMS_.csv'),header=T, dec='.',sep=",")
N= 100

params=colnames(PARAMS)
Y1 = NULL
Y2 = NULL


for (i in 1:nrow(PARAMS))
{
  io =read.csv(paste0(Out_dir,'/SA_FMC_',i,'.csv'),header=T, dec='.',sep="")
  Y1[i]  = io$yearly_FMCCanopy_min
  Y2[i]  = io$yearly_LFMC_min
}
R=10e3
type <- "norm"
conf <- 0.95

IND1[[1]] <- sobol_indices(Y = Y1, N = N, params = params, order="first",boot = F)
IND1[[2]] <- sobol_indices(Y = Y2, N = N, params = params, order="first",boot = F)


AAA = sobol_indices(Y=Y1,N=N,params=params,R=95,boot=T)
# as.data.frame(AAA)
# BBB = as.data.frame(print(A))
# write.csv(x=BBB,file='Nomdufichier.csv',sep=';',dec='.')
# 

plot(IND1[[1]])
# plot(IND1[[2]])



#GRILLE_V=c(0.15,0.43,0.71,0.99)
#GRILLE_H = c(0.11,0.48,0.55,0.92)


ORDER = params[c(7,8,9,2,5,1,4,6,3)]

params
ORDER
mylist1=NULL
for (i in 1:length(ORDER))
{
  index_1 = which(IND1[[1]]$results$parameters==ORDER[i])[1]
  index_2 = which(IND1[[1]]$results$parameters==ORDER[i])[2]
  mylist1[[i]] <-  cbind(IND1[[1]]$results$original[index_2],
                         IND1[[2]]$results$original[index_2])
}

AAA = multistack.bar(mylist1,col=F,border=F,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.4))
abline(h=seq(.1,.4,.1),lty=3,lwd=0.5,col='grey70')
AAA = multistack.bar(mylist1,col=c(1,2),border=F,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.5),add=T)

# filename=  paste0(getwd(),'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_AdjustedAllYears.csv')
# DATA = read.csv(filename,header=T, dec='.',sep="")
# DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
# library(lubridate)
# #DATA= DATA[year(DATA$Time==2017),]
# A =yday(DATA$Time) + hour(DATA$Time) /24
 
# #plot(A[19521: 29280],DATA$FMCCanopy[19521: 29280],type='l')
 
 CCC=c(gray(.5,1),"#DA7C30")
# plot(1:2,1:2,col=CCC,pch=16,cex=5)
 

# FIGURE ####
graphics.off()
quartz(height=3,width=6.3)
plot.new()
# par(plt=c(0.7,0.95,0.7,0.95),lheight=.85)
# 
# plot(A[19521: 29280],DATA$FMCCanopy[19521: 29280],type='n',axes=F,xlab='',ylab='',col='brown')
# box(lwd=0.5)
# mtext(side=2,"CMC (%)",cex=0.6,line=0.5)
# mtext(side=1,"Days",cex=0.65,line=0)
# abline(h=67)
# lines(A[19521: 29280],DATA$FMCCanopy[19521: 29280],col=gray(.3,1),lwd=0.6)
#axis(1,tck =0.03,at=seq(0,100,20),labels=F,lwd=0,lwd.tick=0.5,cex.axis=0.65, mgp=c(0,0,0))


par(new=T,plt=c(0.1,.95,0.3,0.95),lwd=0.5,xpd=F)
AAA = multistack.bar(mylist1,col=F,border=F,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.47))
segments(x0=rep(-10,6),x1 = rep(42,6),y0=seq(.1,.7,.1),y1=seq(.1,.7,.1),lty=3,lwd=0.8,col='grey70')
AAA = multistack.bar(mylist1,col=CCC,border=F,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.47),add=T)


mtext(side=2,"Global sensitivity index",cex=.8,line=1.5)
axis(1,cex.axis=0.7,tck=-0.03,at=c(-10,AAA[seq(1,50,2)]+0.65),lwd=0,lwd.ticks=.5,labels=F,xlim=c(0,10))
axis(1,cex.axis=0.7,tck=-0,at=c(-10,46),lwd=0.5,labels=F,xlim=c(0,10))
axis(2,las=2,cex.axis=0.7,tck=-0.02,lwd=0.5,mgp=c(0,0.4,0))
#box(lwd=.5)




AZER = c('LAI',
         'TAW',
         expression(paste(g[cuti])),
         expression(paste(Q['10a'])),
         expression(paste(Psi[gs50])),
         expression(paste(P['50'])),
         expression(paste(pi['0'])),
         expression(paste(epsilon)),
         expression(paste(alpha['f'])))





text(x = AAA[seq(2,66,2)],
     ## Move labels to just below bottom of chart.
     y =-0.05,  
     ## Use names from the data list.
     labels = AZER,
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 40,
     ## Adjust the labels to almost 100% right-justified.
     #adj = 0.965,
     adj=0.5,
     ## Increase label size.
     cex = 1)

legend(x=26,y=0.36,bg='white',pch = 22, pt.bg=CCC,col=F,legend=c(expression(paste(CFMC[min],'  (Canopy)')),expression(paste(LFMC[min],'  (Live leaves)'))),pt.lwd=3,cex=0.8)


library(pBrackets)
off_b = 1
YYY = -0.09
brackets(x1=AAA[1]-off_b, x2=AAA[10] + off_b,  y1=YYY, y2=YYY, lwd=1.3,type=1,h=-0.03,xpd=T)
#brackets(x1=AAA[5]-off_b, x2=AAA[6]+off_b, y1=YYY, y2=YYY, lwd=1.3,type=1,h=-0.03,xpd=T)
brackets(x1=AAA[11]-off_b, x2=AAA[12]+off_b, y1=YYY, y2=YYY, lwd=1.3,type=1,h=-0.03,xpd=T)
brackets(x1=AAA[13]-off_b, x2=AAA[18]+off_b, y1=YYY, y2=YYY, lwd=1.3,type=1,h=-0.03,xpd=T)
#brackets(x1=AAA[15]-off_b, x2=AAA[18]+off_b, y1=YYY, y2=YYY, lwd=1.3,type=1,h=-0.03,xpd=T)

par(xpd=T)
par(lheight=.8)
Y22=0.025
text('Water use\nand regulation ',font=4,x=(AAA[1]+AAA[10])/2,y=YYY-Y22,cex=0.8,pos=1,offset=0.6)
#text('regulation\n',font=4,x=(AAA[5]+AAA[6])/2,y=YYY-Y22,cex=0.8,pos=1,offset=0.6)
text('Vulnerability\nto cavitation'    ,font=4,x=(AAA[11]+AAA[12])/2,y=YYY-Y22,cex=0.8,pos=1,offset=0.6)
text('p-v curves' ,font=4,x=(AAA[13]+AAA[18])/2,y=YYY-Y22,cex=0.8,pos=1,offset=0.6)
#text('Water\nleaks',font=4,x=(AAA[15]+AAA[18])/2,y=YYY-Y22,cex=0.8,pos=1,offset=0.6)





