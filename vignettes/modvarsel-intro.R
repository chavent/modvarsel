## ----message=FALSE-------------------------------------------------------
library(modvarsel)
data(simus)
X <- simus$X # matrix of input variables (predictors)
Y <- simus$Y1 # output vector (response)

## ----eval=FALSE----------------------------------------------------------
#  ?choicemod
#  res <- choicemod(X,Y,method=c("linreg","sir","rf"), N =50, nperm=100)
#  # The computation time a bit long. So we use the results stored in the the dataset simus
#  res <- simus$res1

## ----echo=FALSE----------------------------------------------------------
res <- simus$res1

## ------------------------------------------------------------------------
head(res$mse) # First 6 reduced models (over 50)
head(res$mse_all) # First 6 complete models (over 50)

## ------------------------------------------------------------------------
head(res$r2) # First 6 reduced models (over 50)
head(res$r2_all) # First 6 complete models (over 50)

## ---- fig.width = 5, fig.height= 4---------------------------------------
boxplot(res, type="reduced", col=rgb(0,0,1,0.2), main="reduced models, N=50")

## ---- fig.width = 5, fig.height= 4---------------------------------------
boxplot(res,type="complete",col=rgb(0,0,1,0.2), main="complete models, N=50")

## ----fig.width = 5, fig.height= 4----------------------------------------
?boxplot.choicemod
boxplot(res, method = c("linreg", "sir"), 
    type = "both", col = rgb(0,1,0,0.2), las = 2,
    main = "N = 50 replications")

## ----fig.width = 5, fig.height= 4----------------------------------------
boxplot(res, method = c("linreg", "sir"), measure = "r2",
    type = "both", col = rgb(0,1,0,0.2), las = 2,
    main = "N = 50 replications")

## ----fig.width = 4, fig.height= 3----------------------------------------
?barplot.choicemod
barplot(res, method="linreg", type="varsel", las = 2, 
  main = "linreg", col = rgb(1,1,0,0.2))

## ----fig.width = 4, fig.height= 3----------------------------------------
barplot(res, method="linreg", type="sizemod", las = 2, 
  main = "linreg", col = rgb(0,1,1,0.2))

## ----fig.width = 4, fig.height= 3----------------------------------------
barplot(res, method="sir", type="varsel", las = 2, 
  main = "sir", col = rgb(1,1,0,0.2))

## ----fig.width = 4, fig.height= 3----------------------------------------
barplot(res, method="sir", type="sizemod", las = 2, 
  main = "sir", col = rgb(0,1,1,0.2))

## ------------------------------------------------------------------------
imp <- varimportance(X,Y,method="linreg",nperm=100)

## ------------------------------------------------------------------------
#importance of the variables for the first 6 permutations (over 100)
knitr::kable(head(imp$mat_imp),digits=2) 

## ------------------------------------------------------------------------
meanvi <- colMeans(imp$mat_imp) # mean importance
knitr::kable(t(meanvi),digits=2)

## ------------------------------------------------------------------------
knitr::kable(imp$base_imp, digit=2) # baseline value of importance

## ----fig.width=5, fig.height=4-------------------------------------------
?plot.varimportance
plot(imp,choice="boxplot", col="lightgreen") #by default

## ----fig.width=5, fig.height=4-------------------------------------------
plot(imp,choice="meanplot", cutoff = FALSE)

## ----fig.width=5, fig.height=4-------------------------------------------
plot(imp,choice="meanplot", cutoff = TRUE)

## ------------------------------------------------------------------------
?select.varimportance
select(imp, cutoff=TRUE) # by default

## ------------------------------------------------------------------------
select(imp, cutoff=FALSE, nbsel=7)

## ------------------------------------------------------------------------
Xnew <- simus$Xnew

## ------------------------------------------------------------------------
sel <- select(imp, cutoff=TRUE)$indices

## ------------------------------------------------------------------------
X <- X[, sel] 
knitr::kable(head(X),digits=2) # First 6 lines of the reduced input matrix
Xnew <- Xnew[, sel] # reduced new data matrix
knitr::kable(Xnew,digits=2)

## ------------------------------------------------------------------------
#estimate the linear model
m1 <- lm(Y~.,data = data.frame(X))
#predict the input data
Ypred <- predict(m1,newdata = data.frame(X))

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
#plot
plot(Y,Ypred, main = "linreg")

## ------------------------------------------------------------------------
#predict the response of the 10 new observations
newpred1 <- predict(m1,newdata = data.frame(Xnew))
knitr::kable(t(newpred1),digits=2)

## ------------------------------------------------------------------------
# SIR
beta <- edrGraphicalTools::edr(Y, X,
        H = 10, K = 1, method = "SIR-I")$matEDR[, 1, drop = FALSE]
# Single index
index <- X %*% beta

# Bandwith tuning
hopt <- cv_bandwidth(index, Y, graph.CV=FALSE)$hopt

# Prediction of the input data
Ypred <-ksmooth(index, Y, kernel = "normal",
        bandwidth = hopt, x.points = index)$y[rank(index)]

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
plot(Y,Ypred, main = "sir+kernel")

## ------------------------------------------------------------------------
indexnew <- Xnew%*%beta
newpred2 <- ksmooth(index, Y, kernel = "normal",
        bandwidth = hopt, x.points = indexnew)$y[rank(indexnew)]
knitr::kable(t(newpred2),digits=2)

## ------------------------------------------------------------------------
#tune mtry
bestmtry <-randomForest::tuneRF(X, Y,trace=FALSE,plot=FALSE)
#build the forest
m3 <- randomForest::randomForest(X, Y, mtry = bestmtry)
# predict the input data
Ypred <- predict(m3,newdata = X, type = "response")

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
plot(Y,Ypred, main = "random forest")

## ------------------------------------------------------------------------
newpred3 <- predict(m3,newdata = Xnew, type = "response")
knitr::kable(t(newpred3),digits=2)

## ----fig.width=7, fig.height=4-------------------------------------------
matplot(cbind(newpred1,newpred2,newpred3), ylab="predictions", 
        xlab = "new observations",  cex=0.8)
legend("bottomleft", inset=0.01, legend=c("linreg","sir","rf"), col=c(1:3), pch=c("1","2","3"), cex=0.8)

## ------------------------------------------------------------------------
data("psbst")
X <- psbst$dat[,-22] #  matrix of predictors
Y <- psbst$dat$WB55a14J # response variable

## ----eval=FALSE----------------------------------------------------------
#  res <- choicemod(X, Y, N=50,nperm=100)
#  # The computation time a bit long. So we use the results stored in the the dataset psbst
#  res <- psbst$res

## ----echo=FALSE----------------------------------------------------------
res <- psbst$res

## ----fig.width = 5, fig.height= 4----------------------------------------
boxplot(res,
    type = "both", col = rgb(0,1,0,0.2), las = 2,
    main = "N = 50 replications")

## ----fig.width = 5, fig.height= 4----------------------------------------
boxplot(res, ylim=c(0,220),
    type = "both", col = rgb(0,1,0,0.2), las = 2,
    main = "N = 50 replications")

## ----fig.width = 6, fig.height= 5----------------------------------------
barplot(res, method="linreg", type="varsel", las = 2, 
  main = "linreg", col = rgb(1,1,0,0.2))

## ----fig.width = 6, fig.height= 5----------------------------------------
barplot(res, method="linreg", type="sizemod", las = 2, 
  main = "linreg", col = rgb(0,1,1,0.2))

## ----fig.width = 6, fig.height= 5----------------------------------------
barplot(res, method="sir", type="varsel", las = 2, 
  main = "sir", col = rgb(1,1,0,0.2))

## ----fig.width = 6, fig.height= 5----------------------------------------
barplot(res, method="sir", type="sizemod", las = 2, 
  main = "sir", col = rgb(0,1,1,0.2))

## ----fig.width = 6, fig.height= 5----------------------------------------
barplot(res, method="rf", type="varsel", las = 2, 
  main = "rf", col = rgb(1,1,0,0.2))

## ----fig.width = 6, fig.height= 5----------------------------------------
barplot(res, method="rf", type="sizemod", las = 2, 
  main = "rf", col = rgb(0,1,1,0.2))

## ----eval=FALSE----------------------------------------------------------
#  imp1 <- varimportance(X,Y,method="linreg",nperm=200)

## ----echo=FALSE----------------------------------------------------------
imp1 <- psbst$imp1

## ----fig.width=6, fig.height=5-------------------------------------------
plot(imp1,choice="boxplot", col="lightgreen") #by default

## ----fig.width=6, fig.height=5-------------------------------------------
plot(imp1,choice="meanplot", cutoff = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  imp2 <- varimportance(X,Y,method="sir",nperm=200)

## ----echo=FALSE----------------------------------------------------------
imp2 <- psbst$imp2

## ----fig.width=6, fig.height=5-------------------------------------------
plot(imp2,choice="boxplot", col="lightgreen") #by default

## ----fig.width=6, fig.height=5-------------------------------------------
plot(imp2,choice="meanplot", cutoff = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  imp3 <- varimportance(X,Y,method="rf",nperm=200)

## ----echo=FALSE----------------------------------------------------------
imp3 <- psbst$imp3

## ----fig.width=6, fig.height=5-------------------------------------------
plot(imp3,choice="boxplot", col="lightgreen") #by default

## ----fig.width=6, fig.height=5-------------------------------------------
plot(imp3,choice="meanplot", cutoff = TRUE)

