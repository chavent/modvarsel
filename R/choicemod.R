#' Choice of the regression model
#'
#' This function estimates the mean squared error (MSE) of parametric,
#' semi-parametric or non parametric regression models (including possibly
#' covariates selection)  using a repeated learning/test samples approach.
#' The models are estimated with different methods (chosen by the user) for
#' comparison purpose. The following methods (with and without variables
#' selection) are available: multiple linear regression (\code{linreg}),
#' sliced inverse regression associated with kernel regression (\code{sir}),
#' random forests regression (\code{rf}), principal components regression  (\code{pcr}),
#' partial least squares regression (\code{plsr}), ridge regression (\code{ridge}).
#' The procedure for covariates selection is the same for all the
#' estimation methods and is based on variable importance (VI) obtained via
#' repeated random perturbations of the covariates.
#'
#'
#' @param X a numerical matrix containing the \code{p} variables in the model.
#' @param Y a numerical response vector.
#' @param method a vector with the names of the chosen regression methods
#' (\code{"linreg"}, \code{"sir"}, \code{"rf"}, \code{"pcr"}, \code{"plsr"}, \code{"ridge"}).
#' @param N the number of replications (the number of ramdom leaning/test
#' samples) to estimate the MSE values.
#' @param prop_train a value between 0 and 1 with the proportion of
#' observations in the training samples.
#' @param nperm the number of random permutations to perform the importance of the covariates (VI).
#' @param cutoff if TRUE the covariates are selected automatically and the number
#'  of selected variables is unknown. If \code{cutoff=FALSE} the \code{nbsel}
#'   best variables are selected.
#' @param nbsel the number of selected covariates. Active only if
#' \code{cutoff=FALSE}.
#'
#' @return An object with S3 class "choicemod" and the following components:
#' \item{mse}{a matrix of dimension \code{N} times \code{length(methods)}
#'  with the values of MSE calculated with the \code{N} test samples (in row)
#'  and each regression method (in column) estimating the reduced models (with covariate selection)
#'   on the training samples.}
#' \item{mse_all}{a matrix of dimension \code{N} times \code{length(methods)}
#'  with the values of MSE calculated with the \code{N} test samples (in rows)
#'  and each regression method (in columns) estimating the complete models (no covariate selection)
#'   on the training samples.}
#' \item{sizemod}{a matrix of dimension \code{N} times  \code{length(methods)}
#' with the number of covariates selected in the reduced model for each replication (in row)
#' and each regresion method (in column).}
#' \item{pvarsel}{a matrix of dimension \code{p} times \code{length(methods)}
#' with the occurrences (in percent) of selection of each covariates (in row)
#' and each regression method (in column).}
#'
#' @seealso  \code{\link{boxplot.choicemod}}, \code{\link{barplot.choicemod}},
#' \code{\link{varimportance}}
#'
#' @examples
#' data(simus)
#' X <- simus$X
#' Y <- simus$Y1
#' #res <- choicemod(X,Y,method=c("linreg","sir"), N = 50, nperm = 100)
#' #The computation time a bit long. So the results have been stored.
#' res <- simus$res1
#' boxplot(res)
#'
#' @details The only method with no parameter to tune is \code{"linreg"}.
#' The parameters of the methods \code{sir}, \code{pcr}, \code{plsr} and \code{ridge}
#' are tuned on the training samples. The bandwidth for Kernel Regression Smoother is
#'  tuned by leave one out cross validation. The number of components for \code{pcr} and \code{plsr} is
#'  tuned as follows: for each possible number of components, the root mean square error (RMSE) is calculated
#'  via 5-fold cross validation  and the number of components is selected by detecting
#'  a change point position (in mean and variance). The parameter \code{mtry} for random forests
#'  regression is not tuned and is fixed to p/3. The number of trees is not tuned and is fixed to \code{ntree=300}.
#'
#' @export


choicemod <- function(X, Y, method = c("linreg","sir","rf"), N = 20,
                      prop_train = 0.8, nperm = 50,
                      cutoff=TRUE, nbsel=NULL,ntree=300, parallel=TRUE, numCores=parallel::detectCores()){
  if (!(all(method %in% c("linreg", "sir", "rf", "pcr", "plsr", "ridge"))))
    stop("The argument \"method\" allows \"linreg\", \"sir\", \"rf\", \"pcr\", \"plsr\", \"ridge\"",
      call. = FALSE)
  if (!(cutoff %in% c(TRUE, FALSE)))
    stop("\"cutoff\" must be either \"TRUE\" or \"FALSE\"",
      call. = FALSE)
  if ((cutoff == FALSE) && (is.null(nbsel)))
    stop("If \"cutoff=FALSE\" the value \"nbsel\"
      must specified", call. = FALSE)
  if (!is.null(nbsel))
    if ( !(nbsel>0) ||  !(nbsel <= ncol(X)) ||
        !(all.equal(nbsel, as.integer(nbsel))))
      stop("\"nbsel\" must be a positive integer
        between 1 and p", call. = FALSE)
  
  if(parallel){registerDoParallel(numCores)}
  X <- as.matrix(X)
  n <- nrow(X)
  n_train <- round(prop_train*n, digits = 0)
  n_test <- n-n_train

  mse_linreg <- rep(0,N)
  varsel_linreg <- list()
  mse_linreg_c <- rep(0,N)

  mse_sir <- rep(0,N)
  varsel_sir <- list()
  mse_sir_c <- rep(0,N)

  mse_rf <- rep(0,N)
  varsel_rf <- list()
  mse_rf_c <- rep(0,N)

  mse_pcr <- rep(0,N)
  varsel_pcr <- list()
  mse_pcr_c <- rep(0,N)

  mse_plsr <- rep(0,N)
  varsel_plsr <- list()
  mse_plsr_c <- rep(0,N)

  mse_ridge <- rep(0,N)
  varsel_ridge <- list()
  mse_ridge_c <- rep(0,N)


  #--replications of train/test sets
  for (i in 1:N){

    cat("Replication ", i," on ", N, fill = TRUE)

    train <- sample(1:n, size = n_train, replace = FALSE)
    Xtrain <- X[train,]
    Ytrain <- Y[train]
    Xtest <- X[-train,]
    Ytest <- Y[-train]
    Y.pred.RegLin <- rep(0,n_test)
    Y.pred.SIR <- rep(0,n_test)
    Y.pred.RF <- rep(0,n_test)
    #========
    # RegLin
    #========
    if ("linreg" %in% method){
      #with variables selection
      imp <- varimportance(Xtrain, Ytrain, method = "linreg",
                           nperm=nperm,
                          parallel=parallel,numCores=numCores)
      selvar <- select.varimportance(imp, cutoff = cutoff,
        nbsel = nbsel)
      Xtest_sel <- Xtest[,selvar$indices, drop=FALSE]
      Xtrain_sel <- Xtrain[,selvar$indices, drop=FALSE]
      model <- stats::lm(Ytrain~.,data = data.frame(Xtrain_sel))
      Ypred <- stats::predict(model,newdata = data.frame(Xtest_sel))

      mse_linreg[i] <- mean((Ytest-Ypred)^2)
      varsel_linreg[[i]] <- selvar$var

      #with all available variables
      model <- stats::lm(Ytrain~.,data = data.frame(Xtrain))
      Ypred <- stats::predict(model,newdata = data.frame(Xtest))

      mse_linreg_c[i] <- mean((Ytest-Ypred)^2)
    }
    #========
    # SIR
    #========
    if ("sir" %in% method){
      #with variables selection
      imp <- varimportance(Xtrain, Ytrain, method = "sir",
                           nperm=nperm,
                          parallel=parallel,numCores=numCores)
      selvar <- select.varimportance(imp, cutoff = cutoff,
        nbsel = nbsel)
      Xtest_sel <- Xtest[,selvar$indices, drop=FALSE]
      Xtrain_sel <- Xtrain[,selvar$indices, drop=FALSE]
      beta <- edrGraphicalTools::edr(Ytrain, Xtrain_sel,
        H = 10, K = 1, method = "SIR-I")$matEDR[, 1,
          drop = FALSE]
      indice <- Xtrain_sel%*%beta
      hopt <- cv_bandwidth(indice, Ytrain,graph.CV=FALSE)$hopt
      indicetest <- Xtest_sel%*%beta
      mat <- cbind(Ytest,indicetest)
      matord <- mat[order(mat[,2]),]
      Ytest_ord <- matord[,1]
      indicetest_ord <- matord[,2]
      Ypred <- stats::ksmooth(indice, Ytrain, kernel = "normal",
        bandwidth = hopt,
        x.points = indicetest_ord)$y

      mse_sir[i] <- mean((Ytest_ord-Ypred)^2)
      varsel_sir[[i]] <- selvar$var

      #with all available variables
      beta <- edrGraphicalTools::edr(Ytrain, Xtrain,
        H = 10, K = 1, method = "SIR-I")$matEDR[, 1,
          drop = FALSE]
      indice <- Xtrain%*%beta
      hopt <- cv_bandwidth(indice, Ytrain,graph.CV=FALSE)$hopt
      indicetest <- Xtest%*%beta
      mat <- cbind(Ytest,indicetest)
      matord <- mat[order(mat[,2]),]
      Ytest_ord <- matord[,1]
      indicetest_ord <- matord[,2]
      Ypred <- stats::ksmooth(indice, Ytrain, kernel = "normal",
        bandwidth = hopt,
        x.points = indicetest_ord)$y

      mse_sir_c[i] <- mean((Ytest_ord-Ypred)^2)
    }
    #================
    # Random Forests
    #================
    if ("rf" %in% method){
      #with variables selection
      imp <- varimportance(Xtrain, Ytrain, method = "rf",
        nperm=nperm,ntree=ntree,
                          parallel=parallel,numCores=numCores)
      selvar <- select(imp, cutoff = cutoff,
        nbsel = nbsel)
      Xtest_sel <- Xtest[,selvar$indices, drop=FALSE]
      Xtrain_sel <- Xtrain[,selvar$indices, drop=FALSE]

      model <- randomForest::randomForest(x = Xtrain_sel,
        y = Ytrain, ntree = ntree)
      Ypred2 <-stats::predict(model,newdata = Xtest_sel,
        type = "response")

      mse_rf[i] <- mean((Ytest-Ypred)^2)
      varsel_rf[[i]] <- selvar$var

      #with all available variables
      model <- randomForest::randomForest(x = Xtrain,
        y = Ytrain, ntree = ntree)
      Ypred <- stats::predict(model,newdata = Xtest,
        type = "response")

      mse_rf_c[i] <- mean((Ytest-Ypred)^2)
    }
    #========
    # PCR
    #========
    if ("pcr" %in% method){
      #with variables selection
      imp <- varimportance(Xtrain, Ytrain, method = "pcr",
                           nperm=nperm,
                          parallel=parallel,numCores=numCores)
      selvar <- select.varimportance(imp, cutoff = cutoff,
                                     nbsel = nbsel)
      Xtest_sel <- Xtest[,selvar$indices, drop=FALSE]
      Xtrain_sel <- Xtrain[,selvar$indices, drop=FALSE]

      model <- pls::pcr(Ytrain~., data = data.frame(Xtrain_sel),
        validation = "CV", scale=FALSE)
      # rmsep <- pls::RMSEP(model, intercept = FALSE)$val["CV",,]
      rmsep <- sqrt(model$validation$PRESS/n_train)
      ncomp <- find.cpt(rmsep)
      Ypred <- as.vector(stats::predict(model, data.frame(Xtest_sel),
        ncomp=ncomp))
      mse_pcr[i] <- mean((Ytest - Ypred)^2)
      varsel_pcr[[i]] <- selvar$var

      #with all available variables
      model <- pls::pcr(Ytrain~., data = as.data.frame(Xtrain),
                        validation="CV", scale=FALSE)
      # rmsep <- pls::RMSEP(model, intercept = FALSE)$val["CV",,]
      rmsep <- sqrt(model$validation$PRESS/n_train)
      ncomp <- find.cpt(rmsep)
      Ypred <-  as.vector(stats::predict(model, data.frame(Xtest),
        ncomp=ncomp))

      mse_pcr_c[i] <- mean((Ytest - Ypred)^2)
    }
    #========
    # PLSR
    #========
    if ("plsr" %in% method){
      #with variables selection
      imp <- varimportance(Xtrain, Ytrain, method = "plsr",
                           nperm=nperm,
                          parallel=parallel,numCores=numCores)
      selvar <- select.varimportance(imp, cutoff = cutoff,
                                     nbsel = nbsel)
      Xtest_sel <- Xtest[,selvar$indices, drop=FALSE]
      Xtrain_sel <- Xtrain[,selvar$indices, drop=FALSE]


      model <- pls::plsr(Ytrain~., data = data.frame(Xtrain_sel),
                        validation = "CV", scale=TRUE)
      # rmsep <- pls::RMSEP(model, intercept = FALSE)$val["CV",,]
      rmsep <- sqrt(model$validation$PRESS/n_train)
      ncomp <- find.cpt(rmsep)
      Ypred <- as.vector(stats::predict(model, data.frame(Xtest_sel),
        ncomp=ncomp))

      mse_plsr[i] <- mean((Ytest - Ypred)^2)
      varsel_plsr[[i]] <- selvar$var

      #with all available variables
      model <- pls::plsr(Ytrain~., data = as.data.frame(Xtrain),
                        validation="CV", scale=TRUE)
      # rmsep <- pls::RMSEP(model, intercept = FALSE)$val["CV",,]
      rmsep <- sqrt(model$validation$PRESS/n_train)
      ncomp <- find.cpt(rmsep)
      Ypred <-  as.vector(stats::predict(model, data.frame(Xtest),
        ncomp=ncomp))

      mse_plsr_c[i] <- mean((Ytest - Ypred)^2)
    }
    #========
    # RIDGE
    #========
    if ("ridge" %in% method){

      #with variables selection
      imp <- varimportance(Xtrain, Ytrain, method = "ridge",
                           nperm=nperm,
                          parallel=parallel,numCores=numCores)
      selvar <- select.varimportance(imp, cutoff = cutoff,
                                     nbsel = nbsel)
      Xtest_sel <- Xtest[,selvar$indices, drop=FALSE]
      Xtrain_sel <- Xtrain[,selvar$indices, drop=FALSE]

      model <- glmnet::cv.glmnet(Xtrain_sel, Ytrain,
        family = "gaussian", alpha=0, standardize = FALSE,
        nfolds = 10, grouped=FALSE)
      Ypred <- stats::predict(model, Xtest_sel, s = "lambda.min")
      mse_ridge[i] <- mean((Ytest-Ypred)^2)
      varsel_ridge[[i]] <- selvar$var

      #with all available variables
      model <- glmnet::cv.glmnet(Xtrain, Ytrain,
        family = "gaussian", alpha=0, standardize = FALSE,
        nfolds = 10, grouped=FALSE)
      Ypred <- stats::predict(model, Xtest, s = "lambda.min")
      mse_ridge_c[i]<- mean((Ytest-Ypred)^2)
    }

  }
  mse <- data.frame(linreg = mse_linreg,
    sir = mse_sir, rf = mse_rf, pcr = mse_pcr, plsr = mse_plsr, ridge = mse_ridge)
  mse <- mse[,(c("linreg","sir","rf", "pcr", "plsr", "ridge") %in% method),
    drop=FALSE]

  mse_all <- data.frame(linreg_all = mse_linreg_c,
    sir_all = mse_sir_c, rf_all = mse_rf_c, pcr_all = mse_pcr_c, plsr_all = mse_plsr_c, ridge_all = mse_ridge_c)
  mse_all <- mse_all[,(c("linreg","sir","rf", "pcr", "plsr", "ridge") %in%
      method),drop=FALSE]

  varsel <- list(linreg = varsel_linreg, sir = varsel_sir,
    rf = varsel_rf, pcr = varsel_pcr, plsr = varsel_plsr, ridge = varsel_ridge)
  varsel <- varsel[match(method,c("linreg", "sir", "rf", "pcr", "plsr", "ridge"))]

  #proportion of selection of each variable
  p <- ncol(X)
  pvarsel <- matrix(NA, nrow = p, ncol = length(method))
  rownames(pvarsel) <- colnames(X)
  colnames(pvarsel) <- method
  for (i in 1:length(varsel))
    for (j in 1:p)
      pvarsel[j,i] <- sum(unlist(lapply(varsel[[i]],function(x){colnames(X)[j] %in% x})))/N

  #number of variables selected by the method at each replication
  sizemod <- matrix(NA, nrow = N, ncol = length(method))
  colnames(sizemod) <- method
  for (i in 1:length(varsel))
    sizemod[,i] <- sapply(varsel[[i]], length)

  structure(
    list(mse = mse,
         mse_all  = mse_all,
         sizemod = sizemod,
         pvarsel = pvarsel,
         varsel = varsel,
         method = method,
         N=N,
         nperm=nperm),
    class = "choicemod"
  )
}

