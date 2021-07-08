varimp_clm <- function(X, Y, nrep=10,parallel=FALSE,myCluster=parallel::makeCluster(parallel::detectCores())
){
  
  Yf<-as.factor(Y)
  model<-ordinal::clm(Yf~.,data=data.frame(X))
  Yprob<-stats::predict(model,newdata=data.frame(X),type="prob")$fit
  Yvalue<-as.numeric(colnames(Yprob))
  Ypred<-Y*0
  for (i in 1:length(Yvalue)){Ypred<-Ypred+Yprob[,i]*Yvalue[i]}
  
  base_mse <- mean(( Y -Ypred )^2)
  
  # Repetitions of VI measures for each variable
  p <- ncol(X)
  n <- nrow(X)
  mat_mse <- matrix(0,nrow=nrep,ncol=p)
  colnames(mat_mse) <- colnames(X)
  
  if (!parallel){
    for (r in 1:nrep) {
      for (j in 1:p){
        Xperm <- X
        Xperm[,j] <- Xperm[sample(1:n),j]
        res <- ordinal::clm(Yf~.,data=data.frame(Xperm))
        Yprob<-stats::predict(model,newdata=data.frame(Xperm),type="prob")$fit
        Yvalue<-as.numeric(colnames(Yprob))
        Ypred<-Y*0
        for (i in 1:length(Yvalue)){Ypred<-Ypred+Yprob[,i]*Yvalue[i]}
        
        mat_mse[r,j] <-  mean(( Y -Ypred )^2)
        
      }
    }
  }
  else if (parallel){
    doParallel::registerDoParallel(myCluster)
    for (j in 1:p){
        mat_mse[,j]<-foreach::foreach(r =c(1:nrep), .combine = 'c') %dopar% {
        Xperm <- X
        Xperm[,j] <- Xperm[sample(1:n),j]
        res <- ordinal::clm(Yf~.,data=data.frame(Xperm))
        Yprob<-stats::predict(model,newdata=data.frame(Xperm),type="prob")$fit
        Yvalue<-as.numeric(colnames(Yprob))
        Ypred<-Y*0
        for (i in 1:length(Yvalue)){Ypred<-Ypred+Yprob[,i]*Yvalue[i]}
        
        mat_mse[r,j] <-  mean(( Y -Ypred )^2)
      }
    }  
    stopCluster(myCluster)
  }
  list(mat_mse=mat_mse,base_mse=base_mse)
}
