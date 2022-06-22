# Figure sensitivity analysis FMC puechabon 
# Created : 27/02/2022
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
IND2=NULL


directoryToRefSimu = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/sensitivity_FMC/"
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


  
  
  
  

directoryToRefSimu2 = "/Users/jruffault/Dropbox/Mon Mac (MacBook-Pro-de-Julien.local)/Desktop/sensitivity_FMC_2/"
mainDirOutpout2 =directoryToRefSimu2
Out_dir2 <- file.path(mainDirOutpout2)


PARAMS2 =   io =read.csv(paste0(Out_dir2,'/PARAMS_.csv'),header=T, dec='.',sep=",")
N2= 100

params2=colnames(PARAMS2)
Y1 = NULL
Y2 = NULL


for (i in 1:nrow(PARAMS2))
{
  io =read.csv(paste0(Out_dir2,'/SA_FMC_',i,'.csv'),header=T, dec='.',sep="")
  Y1[i]  = io$yearly_FMCCanopy_min
  Y2[i]  =  io$yearly_LFMC_min
}
R=10e3
type <- "norm"
conf <- 0.95

IND2[[1]] <- sobol_indices(Y = Y1, N = N2, params = params2, order="first",boot = F)
IND2[[2]] <- sobol_indices(Y = Y2, N = N2, params = params2, order="first",boot = F)

plot_scatter(Y = Y1, N = N2, data = PARAMS2, params =params2)

plot(IND1[[1]])
plot(IND1[[2]])


plot(IND2[[1]])
plot(IND2[[2]])




#GRILLE_V=c(0.15,0.43,0.71,0.99)
#GRILLE_H = c(0.11,0.48,0.55,0.92)


ORDER = params[c(1,2,3,4,5,6,7,8)]
ORDER2 = params2[c(1,2,3,4,5,6,7,10,8,9)]


mylist1=NULL
for (i in 1:length(ORDER2))
{
  index_1 = which(IND1[[1]]$results$parameters==ORDER2[i])[1]
  index_2 = which(IND1[[1]]$results$parameters==ORDER2[i])[2]
  mylist1[[i]] <-  cbind(IND1[[1]]$results$original[index_2],
                         IND1[[2]]$results$original[index_2])
}

mylist2=NULL
for (i in 1:length(ORDER2))
{
  index_1 = which(IND2[[1]]$results$parameters==ORDER2[i])[1]
  index_2 = which(IND2[[1]]$results$parameters==ORDER2[i])[2]
  mylist2[[i]] <-  cbind(IND2[[1]]$results$original[index_2],
                         IND2[[2]]$results$original[index_2])
}


#AAA = multistack.bar(mylist1,col=F,border=F,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.7))
#abline(h=seq(.1,.4,.1),lty=3,lwd=0.5,col='grey70')
#AAA = multistack.bar(mylist1,col=c(1,2),border=F,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.5),add=T)




filename=  paste0(getwd(),'/Figure_4_validation_LFMC/Puechabon_VG_LFMC_AdjustedAllYears.csv')
DATA = read.csv(filename,header=T, dec='.',sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
library(lubridate)
#DATA= DATA[year(DATA$Time==2017),]
A =yday(DATA$Time) + hour(DATA$Time) /24

#plot(A[19521: 29280],DATA$FMCCanopy[19521: 29280],type='l')






CCC=c(gray(.5,1),"#DA7C30")
plot(1:2,1:2,col=CCC,pch=16,cex=5)



# FIGURE ####
graphics.off()
quartz(height=4,width=6.3)
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


par(new=T,plt=c(0.1,.95,0.6,0.95),lwd=0.5,xpd=F)
AAA = multistack.bar(mylist1,col=F,border=F,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.67))

segments(x0=rep(-10,6),x1 = rep(36.15,6),y0=seq(.1,.7,.1),y1=seq(.1,.7,.1),lty=3,lwd=0.5,col='grey70')
#abline(h=seq(.1,.6,.1),lty=3,lwd=0.5,col='grey70')


AAA = multistack.bar(mylist1,col=CCC,border=F,lwd=.5,betweenspace = 2.5, withinspace = 0.25,beside=T,axes=F,ylim=c(0,0.65),add=T)
axis(2,las=2,cex.axis=0.7,tck=-0.02,lwd=0.5,mgp=c(0,0.4,0))
#box(lwd=.5)

axis(1,cex.axis=0.7,tck=-0.03,at=c(-10,AAA[seq(1,16,2)]+0.65),lwd=0,lwd.ticks=.5,labels=F,xlim=c(0,10))
axis(1,cex.axis=0.7,tck=-0,at=c(-10,37),lwd=0.5,labels=F,xlim=c(0,10))
mtext(side=3,'A',cex=0.8,line=-0,font=2,adj=0)
#legend(x=66,y=0.45,bg='white',fill=CCC,legend=c('Fagus sylvatica','Quercus ilex','Quercus petraea'),cex=0.7,box.lwd=0,col="grey70",text.font=3)
#box(lwd=.5)
abline(v=37,lty=2)

par(xpd=T)
text('Hydraulic traits',y=0.70,adj=0.5,x=(36)/2,font=4,cex=0.85)
mtext(side=2,"Sobol's index",cex=.8,line=1.5)

library(shape)
Arrows(x0 = -1, x1 = 36.65,y0=0.65,y1=0.65,lwd=0.5,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12,col="gray10")  




par(new=T,plt=c(0.1,.95,.15,.5),xpd=F)
AAA = multistack.bar(mylist2,col=F,border=F,betweenspace = 2.5, withinspace=0.25,beside=T,axes=F,ylim=c(0,0.67))
abline(h=seq(.1,.6,.1),lty=3,lwd=0.5,col='grey70')
AAA = multistack.bar(mylist2,col=CCC,border=F,betweenspace = 2.5, withinspace=0.25,beside=T,axes=F,ylim=c(0,0.65),add=T)
axis(2,las=2,cex.axis=0.7,tck=-0.02,lwd=0.5,mgp=c(0,0.4,0))
axis(1,cex.axis=0.7,tck=-0.03,at=c(-10,AAA[seq(1,50,2)]+0.65),lwd=0,lwd.ticks=.5,labels=F,xlim=c(0,10))
axis(1,cex.axis=0.7,tck=-0,at=c(-10,46),lwd=0.5,labels=F,xlim=c(0,10))
mtext(side=3,'B',cex=0.8,line=0,font=2,adj=0)
abline(v=37,lty=2)
abline(v=46,lty=2)

par(xpd=T)
Arrows(x0 = -1, x1 = 36.65,y0=0.65,y1=0.65,lwd=0.5,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12,col='gray10')  
Arrows(x0 = 37.45, x1 = 45.6,y0=0.65,y1=0.65,lwd=0.5,code=3,
       arr.type="triangle", arr.width=0.15,arr.length=0.12,col='gray10')  


text('Hydraulic traits',y=0.70,adj=0.5,x=(36.2)/2,font=4,cex=0.85)
par(lheight=.7)
text('Stand\nparameters',y=0.735,adj=0.5,x=(38+45.7)/2,font=4,cex=0.85,lheight=.8)

mtext(side=2,"Sobol's index",cex=.8,line=1.5)



#### txxt #####
#params2=ORDER

# params3 = c('LAI',
#             'TAW',
#             expression(paste(g[cuti20])),
#             expression(paste(Psi[gs50])),
#             expression(paste(g[canopy])),
#             expression(paste(Q['10a'])),
#             expression(paste(Psi['50L'])),
#             expression(paste(V[S])),
#             expression(paste(beta)),
#             expression(paste(slope[L])),
#             expression(paste(slope[gs])),
#             expression(paste(k[SLApo_max])),
#             expression(paste(k[RSApo_max])),
#             expression(paste(k[LSym])),
#             expression(paste(epsilon)),
#             expression(paste(pi['0'])),
#             expression(paste(Q['10b'])))


AZER = c(expression(paste(P['50'])),
         expression(paste(g[cuti])),
         expression(paste(epsilon)),
         expression(paste(pi['0'])),
         expression(paste(Q['10a'])),
         expression(paste(T['P'])),
         expression(paste(alpha['f'])),
         expression(paste(Psi[gs50])),
         'LAI',
         'TAW')
         
         


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
     adj=0.95,
     ## Increase label size.
     cex = 1)

 par(new=T,plt=c(0.79,.93,.65,.9),xpd=F)
 plot(0:10,0:10,axes=F,type='n',xlab='',ylab='')
 legend('right',pch = 22, pt.bg=CCC,col=F,legend=c(expression(CFMC[min]),expression(LFMC[min])),pt.lwd=3,cex=0.8)
# 
