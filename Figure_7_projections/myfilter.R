library(signal)


# Padding approximations from Mann (2008, Smoothing of climate time series revisited)
paddingDouble <- function(values,method,sz1,sz2){ 
  if (method=='none') {return(values)} # no padding
  if (method=='last') {return(c(rep(values[1],sz1),values,rep(values[length(values)],sz2)))} # repeats final and first value 30 times
  if (method=='mean'){ return(c(rep(mean(values[1:sz1/2]),sz1),values,rep(mean(values[(length(values)-sz2/2):length(values)]),sz2)))} # repeats mean from last 15 years 30 times ==> stabilisation of value
  if (method=='slope0') { return(c(values[seq(sz1+1,2,-1)],values,values[seq(length(values)-1,length(values)-sz2,-1)]))} # vertical reflection of data series ==> slope becomes 0
  if (method=='slopecst') { return(c(values[seq(sz1+1,2,-1)]+2*(values[1]-values[seq(sz1+1,2,-1)]),values,values[seq(length(values)-1,length(values)-sz2,-1)]+2*(values[length(values)]-values[seq(length(values)-1,length(values)-sz2,-1)])))} # horizontal+vertical reflection ==> slope is maintained
  if (method=='slope0_cst') { return(c(values[seq(sz1+1,2,-1)],values,values[seq(length(values)-1,length(values)-sz2,-1)]+2*(values[length(values)]-values[seq(length(values)-1,length(values)-sz2,-1)])))} # horizontal+vertical reflection ==> slope is maintained
  if (method=='slope0_mean') { return(c(values[seq(sz1,1,-1)],values,rep(mean(values[(length(values)-sz2/2):length(values)]),sz2)))} # vertical reflection of data series ==> slope becomes 0
}
# Lowpass Butterworth filter
filter_30 = butter(W=1/30,n=1, type="low", plane="z") # lowpass filter on +/- 30 years (use type="pass" for passband filter and "high" for highpass filter)
filter_10 = butter(W=1/10,n=1, type="low", plane="z") # lowpass filter on +/- 10 years (use type="pass" for passband filter and "high" for highpass filter)

#slope 0 partout
#myfilt <- function(values,filter) {sz=round(length(values)/2);return(filtfilt(filt=filter,x=paddingDouble(values,"slope0",sz))[(1+sz):(length(values)+sz)])}
#slope cst partout
#myfilt <- function(values,filter) {sz=round(length(values)/2);return(filtfilt(filt=filter,x=paddingDouble(values,"slopecst",sz))[(1+sz):(length(values)+sz)])}
#slope0 à gauche cst à droite
#myfilt <- function(values,filter) {sz=round(length(values)/2);return(filtfilt(filt=filter,x=paddingDouble(values,"slope0_cst",sz))[(1+sz):(length(values)+sz)])}

# ADAPTIVE
iopt=1;
myfilt <- function(values,filter,iopt) {
  if (length(values)<=1) {return(values)} # constant
  #print(values)
  #print(sd(values))
  if (sd(values)==0) {return(values)} # constant
  #values=x;filter=filter_30
  msebest=1e15
  sz=round(length(values)/2);
  f1= filtfilt(filt=filter,x=paddingDouble(values,"slope0",sz,sz))[(1+sz):(length(values)+sz)]
  if(iopt==2) {return(f1)}
  f2= filtfilt(filt=filter,x=paddingDouble(values,"slope0_cst",sz,sz))[(1+sz):(length(values)+sz)]
  f3= filtfilt(filt=filter,x=paddingDouble(values,"slope0_mean",sz,30))[(1+sz):(length(values)+sz)]
  f=f3;w1f=0;w2f=0;w3f=1;
  sp=0.01;if(iopt==1) {sp=0.1}
  for (w1 in seq(0,1,sp)) {
    for (w2 in seq(0,1-w1,sp)) {
      f4 = w1*f1+w2*f2+(1-w1-w2)*f3
      mseval=var(f4-values)/var(values)
      if(mseval<msebest) {
        f = f4;msebest=mseval;
        w1f=w1;w2f=w2;w3f=1-w1-w2;
      }
    }  
  }
  return(f)
}
