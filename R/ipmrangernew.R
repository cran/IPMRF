
ipmrangernew=function(marbolr,da,ntree){
  #marbolr: randomforest obtained by ranger library
  #da: data frame with the predictors only, not responses
  #ntree: number of trees in the random forest
  
  
  da=as.data.frame(da)
  
  #percentage of use per tree (if 2 times of a total of 2 splits is more than 2 times of a total of three)
  dime=dim(da)
  if(is.null(dime)){pup=matrix(0,nrow=1,ncol=length(da))
  da=t(as.matrix(da))
  dime=dim(da)
  }else{
    pup=matrix(0,nrow=dime[1],ncol=dime[2])} 
  
  
  
  totob=ntree
  
  for (i in 1:ntree){
    #ar=getTree(marbolr,k=i) #sin label
    ar=getTreeranger(marbolr,k=i)
    ob=dim(da)[1]
    
    #which variables used in prediction
    for(j in 1:ob){
      da1=da[j,]
      wv=prevtree(ar,da1)
      pupi=table(wv)/length(wv)
      dond= unique(sort(wv))
      
      pup[j,dond]=pup[j,dond]+ as.numeric(pupi)
    }
  }
  
  pupf=pup/totob
  
  
  
  dimnames(pupf)=dimnames(da)
  
  return(pupf)
}


