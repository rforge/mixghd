DA<-function(train,trainL,test,testL,method="MGHD",max.iter=100,eps=1e-2,q=2,scale=TRUE,seed=12345){
    if (min(trainL)==0) stop('training label equal to 0')

    G=max(trainL)
  if(method=="MGHFA"){

      res=MGHFA(train,G=G,label=trainL,max.iter=max.iter,q=q,scale=scale,eps=eps)
      testmodel=MAPFA(scale(test),gpar=res$model$gpar)
      aritrain=ARI(res$model$map,trainL)
      aritest=ARI(testmodel,testL)
      
    }
  else if(method=="MSGHD"){

    res=MSGHD(train,G=G,max.iter=max.iter,label=trainL,scale=scale,eps=eps,seed=seed)
      testmodel=MAPMS(scale(test),gpar=res$model$gpar)
      aritrain=ARI(res$model$map,trainL)
      aritest=ARI(testmodel,testL)  }
  else if(method=="MCGHD"){
   
      res=MCGHD(train,G=G,max.iter=max.iter,label=trainL,scale=scale,eps=eps,seed=seed)
      testmodel=MAP(scale(test),gpar=res$model$gpar)
      aritrain=ARI(res$model$map,trainL)
      aritest=ARI(testmodel,testL)
      }
  else if(method=="cMSGHD"){
      
      res=cMSGHD(train,G=G,max.iter=max.iter,label=trainL,scale=scale,eps=eps,seed=seed)
      testmodel=MAPMS(scale(test),gpar=res$model$gpar)
      aritrain=ARI(res$model$map,trainL)
      aritest=ARI(testmodel,testL)
  }
  else {

      res=MGHD(train,G=G,max.iter=max.iter,label=trainL,scale=scale,eps=eps,seed=seed)
      testmodel=MAPGH(scale(test),gpar=res$model$gpar)
      aritrain=ARI(res$model$map,trainL)
      aritest=ARI(testmodel,testL)
  }
  return(list(model=res,testMembership=testmodel,ARItest=aritest,ARItrain=aritrain))
}