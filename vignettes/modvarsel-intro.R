## ----message=FALSE-------------------------------------------------------
library(modvarsel)
data(simus)
X <- simus$X # matrix of covariates
Y <- simus$Y1 # response vector

## ----eval=FALSE----------------------------------------------------------
#  ?choicemod
#  res <- choicemod(X, Y, method = c("linreg","sir","rf"), N = 50, nperm = 100)
#  # The computation time a bit long. So we use the results stored in the the dataset simus
#  res <- simus$res1

## ----echo=FALSE----------------------------------------------------------
res <- simus$res1

## ------------------------------------------------------------------------
head(res$mse) # test MSE of the reduced models (6 first iterations over 50)
head(res$mse_all) # test MSE of the complete models (6 first iterations over 50)

## ---- fig.width = 5, fig.height= 4---------------------------------------
?boxplot.choicemod
boxplot(res, type = "reduced", col = rgb(0,0,1,0.2), 
        ylab = "test MSE",
        main = "Models with variables selection, N=50")

## ---- fig.width = 5, fig.height= 4---------------------------------------
boxplot(res,type="complete",col=rgb(0,0,1,0.2), 
        ylab = "test MSE",
        main="Models without variables selection, N=50")

## ----fig.width = 5, fig.height= 4----------------------------------------
boxplot(res, method = c("linreg", "sir"), 
    type = "both", ylab = "test MSE",
    col = rgb(0,1,0,0.2), las = 2,
    main = "N = 50 replications")

## ----fig.width = 4, fig.height= 3----------------------------------------
?barplot.choicemod
barplot(res, method = "linreg", type = "varsel", las = 2, 
  main = "linreg", col = rgb(1,1,0,0.2))

## ----fig.width = 4, fig.height= 3----------------------------------------
barplot(res, method = "linreg", type = "sizemod", las = 2, 
  main = "linreg", col = rgb(0,1,1,0.2))

## ----fig.width = 4, fig.height= 3----------------------------------------
barplot(res, method="sir", type="varsel", las = 2, 
  main = "sir", col = rgb(1,1,0,0.2))

## ----fig.width = 4, fig.height= 3----------------------------------------
barplot(res, method = "sir", type = "sizemod", las = 2, 
  main = "sir", col = rgb(0,1,1,0.2))

## ------------------------------------------------------------------------
imp <- varimportance(X, Y, method = "linreg", nperm = 100)

## ----echo=FALSE----------------------------------------------------------
#options(scipen = 1, digits = 2)
options(digits = 2)

## ------------------------------------------------------------------------
imp$mat_imp[1:5,] # VI for the 5 first replications (over 100)
colMeans(imp$mat_imp) # mean VI of the 15 covariates
imp$base_imp #original MSE taken as baseline

## ----fig.width=5, fig.height=4-------------------------------------------
?plot.varimportance
plot(imp, choice = "boxplot", col = "lightgreen",
     main = "linreg method and nperm=100 permutations")

## ----fig.width=5, fig.height=4-------------------------------------------
plot(imp, choice = "meanplot",
     main = "mean of nperm=100 permutations")

## ----fig.width=5, fig.height=4-------------------------------------------
plot(imp, choice = "meanplot", order = TRUE, cutoff = TRUE,
     main = "ordered mean VI")

## ------------------------------------------------------------------------
?select.varimportance
select(imp, cutoff=TRUE) # by default

## ------------------------------------------------------------------------
select(imp, cutoff=FALSE, nbsel=7)

## ------------------------------------------------------------------------
Xnew <- simus$Xnew

## ------------------------------------------------------------------------
#sel <- select(imp, cutoff=TRUE)$indices
sel <- 1:5

## ------------------------------------------------------------------------
# matrices with the selected covariates
X <- X[, sel] 
Xnew <- Xnew[, sel] 

## ------------------------------------------------------------------------
#estimate the linear model 
m1 <- lm(Y~.,data = data.frame(X))

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
Ypred <- predict(m1,newdata = data.frame(X))
plot(Y,Ypred, main = "linreg")
abline(0,1)

## ------------------------------------------------------------------------
newpred1 <- predict(m1,newdata = data.frame(Xnew))
newpred1

## ------------------------------------------------------------------------
# estimate beta
beta <- edrGraphicalTools::edr(Y, X,
        H = 10, K = 1, 
        method = "SIR-I")$matEDR[, 1, drop = FALSE]
index <- X %*% beta

# tune the bandwidth
hopt <- cv_bandwidth(index, Y, graph.CV = FALSE)$hopt

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
Ypred <-ksmooth(index, Y, kernel = "normal",
        bandwidth = hopt, x.points = index)$y[rank(index)]
plot(Y,Ypred, main = "sir+kernel")
abline(0,1)

## ------------------------------------------------------------------------
indexnew <- Xnew%*%beta
newpred2 <- ksmooth(index, Y, kernel = "normal",
                    bandwidth = hopt, 
                    x.points = indexnew)$y[rank(indexnew)]
newpred2

## ------------------------------------------------------------------------
#tune mtry
bestmtry <-randomForest::tuneRF(X, Y, trace = FALSE, plot  =FALSE)
#build the forest
m3 <- randomForest::randomForest(X, Y, mtry = bestmtry)

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
Ypred <- predict(m3, newdata = X, type = "response")
plot(Y, Ypred, main = "random forest")
abline(0,1)

## ------------------------------------------------------------------------
newpred3 <- predict(m3, newdata = Xnew, type = "response")
newpred3

## ----fig.width=7, fig.height=4-------------------------------------------
matplot(cbind(newpred1,newpred2,newpred3), ylab = "predictions", 
        xlab = "new observations",  cex=0.8)
legend("bottomleft", inset = 0.01, legend = c("linreg","sir","rf"), col = c(1:3), 
       pch = c("1","2","3"), cex=0.8)

## ------------------------------------------------------------------------
# tune the number of components
m4 <- pls::pcr(Y~., data = as.data.frame(X),
                        validation="CV", scale=FALSE)
n <- nrow(X)
mse_cv <- m4$validation$PRESS/n
ncomp <- find.cpt(mse_cv)

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
Ypred <- predict(m4, newdata=X)[ , , ncomp]
plot(Y,Ypred, main = "pcr")
abline(0,1)

## ------------------------------------------------------------------------
newpred4 <- predict(m4, newdata=Xnew)[ , , ncomp]
newpred4

## ------------------------------------------------------------------------
# tune the number of PLS components
m5 <- pls::plsr(Y~., data = as.data.frame(X),
                        validation="CV", scale=FALSE)
n <- nrow(X)
mse_cv <- m5$validation$PRESS/n
ncomp <- find.cpt(mse_cv)

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
Ypred <- predict(m5, newdata=X)[ , , ncomp]
plot(Y,Ypred, main = "plsr")
abline(0,1)

## ------------------------------------------------------------------------
newpred5 <- predict(m4, newdata=Xnew)[ , , ncomp]
newpred5

## ------------------------------------------------------------------------
# tune the regularization parameter
m6 <- glmnet::cv.glmnet(X, Y,
        family = "gaussian", alpha=0, standardize = FALSE,
        nfolds = 10, grouped=FALSE)

## ----fig.width=6, fig.height=6, out.width="70%"--------------------------
Ypred <- predict(m6, X, s = "lambda.min")
plot(Y,Ypred, main = "ridge")
abline(0,1)

## ------------------------------------------------------------------------
newpred5 <- predict(m6, Xnew,  s = "lambda.min")
newpred5

## ------------------------------------------------------------------------
data("psbst")
X <- psbst$dat[,-22] #  matrix of covariates (biomarkers)
Y <- psbst$dat$WB55a14J # response variable (meat tenderness)

## ----eval=FALSE----------------------------------------------------------
#  res <- choicemod(X, Y, method = c("linreg", "sir", "rf"),
#                   N=50, nperm = 100)
#  # The computation time a bit long. So we use the results stored in the the dataset psbst
#  res <- psbst$res

## ----echo=FALSE----------------------------------------------------------
res <- psbst$res

## ----fig.width = 5, fig.height= 4----------------------------------------
boxplot(res, type = "both", 
    main = "N = 50 replications",
    ylab = "test MSE", ylim=c(0,200), 
    col = rgb(0,1,0,0.2), las = 2,)

## ----fig.width = 5, fig.height= 4----------------------------------------
barplot(res, method = "linreg", type = "varsel", 
        main = "linreg", las = 2, col = rgb(1,1,0,0.2))

barplot(res, method = "linreg", type = "sizemod",  
        main = "linreg", las = 2, col = rgb(0,1,1,0.2))

## ----fig.width = 5, fig.height= 4----------------------------------------
barplot(res, method = "sir", type = "varsel",
        main = "sir", las = 2, col = rgb(1,1,0,0.2))
barplot(res, method = "sir", type = "sizemod",
        main = "sir", las = 2, col = rgb(0,1,1,0.2))

## ----fig.width = 5, fig.height= 4----------------------------------------
barplot(res, method = "rf", type = "varsel",
        main = "rf", las = 2, col = rgb(1,1,0,0.2))
barplot(res, method = "rf", type = "sizemod",
        main = "rf", las = 2, col = rgb(0,1,1,0.2))

## ----eval=FALSE----------------------------------------------------------
#  imp1 <- varimportance(X,Y,method="linreg",nperm=200)
#  # The computation time a bit long. So we use the results stored in the the dataset psbst
#  imp1 <- psbst$imp1

## ----echo=FALSE----------------------------------------------------------
imp1 <- psbst$imp1

## ----fig.width=5, fig.height=4-------------------------------------------
colsel <- rep("white", 21)
colsel[c(2:8,12,17,20)] <- "lightgreen"
plot(imp1, choice = "boxplot", 
     main = "linreg", col = colsel) 

## ----fig.width=5, fig.height=4-------------------------------------------
plot(imp1, choice = "meanplot", 
     order = TRUE, cutoff = TRUE, main="linreg")

## ------------------------------------------------------------------------
select(imp1, nbsel = 6)$var

## ----eval=FALSE----------------------------------------------------------
#  imp2 <- varimportance(X, Y, method = "sir", nperm = 200)
#  imp3 <- varimportance(X, Y, method = "rf", nperm = 200)
#  # The computation time a bit long. So we use the results stored in the the dataset psbst
#  imp2 <- psbst$imp2
#  imp3 <- psbst$imp3

## ----echo=FALSE----------------------------------------------------------
imp2 <- psbst$imp2
imp3 <- psbst$imp3

## ----fig.width=5, fig.height=4-------------------------------------------
plot(imp2, choice = "boxplot", 
     col = "lightgreen", main = 'sir') 
plot(imp2, choice = "meanplot", 
     order = TRUE, cutoff = TRUE, main = 'sir')

## ----fig.width=5, fig.height=4-------------------------------------------
plot(imp3 ,choice = "boxplot", 
     col = "lightgreen", main = 'rf') 
plot(imp3, choice = "meanplot", 
     order = TRUE, cutoff = TRUE, main = 'rf')

