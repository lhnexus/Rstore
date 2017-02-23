drawHist=function(data,ithreshold){
  
  ##par(mfrow = c(1,2))
  gTrans=data*255
  
  gHist=hist(gTrans,plot=FALSE)
  
  
  plot(data,main="Material fail rate with threshold",xlab="LOT ID",ylab="fail rate[%]",ylim=c(0,max(c(ithreshold,data))))
  abline(h=ithreshold,col="red")
  desc_thresh<-paste("threshold=",round(ithreshold,3),sep="")
  legend("topright",legend=c(desc_thresh),col=c(2),lty=1)
  
  hist(gTrans,main="Histogram of Data",xlab="Data Scope",ylab="Density",xlim=c(min(gHist$breaks),max(gHist$breaks)*2))
  
  mTrans=mean(gTrans)
  
  sdTrans=sd(gTrans)
  
  abline(v=ithreshold*255,col="red")
  
  abline(v=mTrans+sdTrans*2.5,col="blue")
  
  abline(v=mTrans-sdTrans*2.5,col="pink")
  
  abline(v=mTrans+sdTrans*5,col="green")
  
  abline(v=mTrans-sdTrans*5,col="yellow")
  
  
  legend("topright",legend=c("threshold","central upper limit","unbalance upper limit","central lower limit","unbalance lower limit"),col=c(2,4,3,6,7),lty=1)
  
}
