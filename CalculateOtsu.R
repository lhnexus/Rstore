calculateOtsu<-function(allGreyValues){
  
  message("Otsu threshold")
  
 
  gTrans=allGreyValues*255
  
  
  gTrans=round(gTrans)
  sdbenchMark<-2
  ## check greyvalue volumn must >1
  if(length(gTrans)<2){
    threshold = allGreyValues*sdbenchMark
    return(threshold)
  }
  
  
  "h=hist(as.vector(gTrans))"
  
  t=tabulate(gTrans)
  
  ####check balance
  
  rHist=hist(gTrans,plot=FALSE)
  
  mHist=mean(gTrans)
  
  sdHist=sd(gTrans)
  
  runOtsu=TRUE
  
  ##sd threshold
  
  ubSD = 5
  
  tcSD = 2.5
  
  ##zz=array()
  
  ##unbalance check
  
  unbalanceUp=mHist+sdHist*ubSD<max(rHist$breaks)
  
  unbalanceDown=mHist-sdHist*ubSD>min(rHist$breaks)
  
  if(unbalanceUp+unbalanceDown>0)
    
    runOtsu=FALSE
  
  ##too centralized check
  
  ##if(rHist$density>mean(rHist$density)){
  
  ##  centralUp=mHist+sdHist*tcSD>rHist$breaks[max(which(rHist$density>mean(rHist$density)))]
  
  ##  centralDown=mHist-sdHist*tcSD<rHist$breaks[min(which(rHist$density>mean(rHist$density)))]
  
  ##}
  ##else{
  ##  centralUp=mHist+sdHist*tcSD>max(rHist$breaks)
  ##  centralDown=mHist-sdHist*tcSD<min(rHist$breaks)
  
  ##}
  
  
  centralUp=mHist+sdHist*tcSD>max(rHist$breaks)
  
  centralDown=mHist-sdHist*tcSD<min(rHist$breaks)
  
  
  if(centralUp+centralDown >1)
    
    runOtsu=FALSE
  
  ##for(i in 1: length(rHist$counts)){
  
  ## tmpdiv = abs(rHist$counts[i]-mHist)-sdHist*3
  
  ## if(tmpdiv>0){
  
  ## runOtsu=FALSE
  
  ## break
  
  ## }
  
  ##
  
  ## }
  
  
  t=t+0.001
  
  numPixel=length(allGreyValues)
  
  ni=t
  
  pi=ni/numPixel
  
  uT=function(p,L){
    
    res=0
    
    for (i in 1:L){
      
      res = res+i*p[i]
      
    }
    
    res
    
  }
  
  wK = function(p,k){
    
    res=0
    
    for (i in 1:k){
      
      res = res+p[i]
      
    }
    
    res
    
  }
  
  uK = function(p,k){
    
    res=0
    
    for (i in 1:k){
      
      res = res+i*p[i]
      
    }
    
    res
    
  }
  
  maxT=function(p,L,k){
    
    (uT(p,L)*wK(p,k)-uK(p,k))^2/(wK(p,k)*(1-wK(p,k)))
    
  }
  
  
  if(length(t)>1){
    
    ##for (i in 1:length(t)){
    
    ## tmp[i]=(uT(pi,length(t))*wK(pi,i)-uK(pi,i))^2/(wK(pi,i)*(1-wK(pi,i)))
    
    ##}
    
    ##o=which.max(abs(tmp-mean(tmp)))
    
    ##threshold=o/255
    
    if(runOtsu){
      
      message("Running Otsu.")
      
      o=optimize(maxT,c(1,length(t)),maximum=TRUE,L=length(t),p=pi)
      
      threshold=o$maximum/255
      
    }
    
    else{
      
      message("Running sd.")
      
      threshold = mean(allGreyValues)+sd(allGreyValues)*sdbenchMark
      
    }
    
    
  }else{
    
    message("Running sd.")
    
    threshold = mean(allGreyValues)+sd(allGreyValues)*sdbenchMark
    
  }
  
  
}



batchCalculateOtsu<-function(dataset){

  
  mid = c(which(diff(dataset$MATERIAL_ID_INDEX)==1),length(dataset$MATERIAL_ID_INDEX))
  result <- data.frame(MATERIAL_ID_INDEX=0,THRESHOLD=0,RUNID=0)
  result <- result[-1,]
  start<-1
  offset<-0
  for(i in 1:length(mid)){
    end<-mid[i]
    rid = c(which(diff(dataset$RUNID[start:end])==1),length(dataset$RUNID[start:end]))+offset
    istart <-offset+1
    for(j in 1:length(rid)){
      statistics <- dataset$THRESHOLD[istart:rid[j]]
      threshold <- calculateOtsu(statistics)
      result<-rbind(result, c(i,j,threshold))
      istart<-rid[j]+1
    }
    start<-mid[i]+1
    offset<-mid[i]
  }
  colnames(result) = c("MATERIAL_ID_INDEX","THRESHOLD","RUNID")
  return(result)
}
