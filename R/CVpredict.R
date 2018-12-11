



#' A predict generic function for condvis
#'
#' @param fit A fitted model
#' @param newdata Where to calculate predictions.
#' @param ... extra arguments to predict
#' @param ptype One of "pred","prob" or "probmatrix"
#' @param pthreshold Used for calculating classes from probs, in the two class case
#' @param ylevels The levels of the response, when it is a factor
#' @param ptrans A function to apply to the result
#' @param type For some predict methods
#' @param n.trees Used by CVpredict.gbm, passed to predict
#' @param s Used by CVpredict.glmnet and CVpredict.cv.glmnet, passed to predict
#' @param makex Used by CVpredict.glmnet and CVpredict.cv.glmnet. A function to construct xmatrix for predict.
#' @param batch_size Used by CVpredict.keras.engine.training.Model, passed to predict
#' @param response Used by CVpredict.keras.engine.training.Model. Name of response (optional)
#' @param predictors Used by CVpredict.keras.engine.training.Model. Name of predictors
#' @return a vector of predictions, or a matrix when type is "probmatrix"
#' @details This is a wrapper for predict used by condvis.
#' When the model response is numeric, the result is a vector of predictions.
#' When the model response is a factor the result depends on the value of ptype.
#' If ptype="pred", the result is a factor.
#' If also threshold is numeric, it is used to threshold a numeric prediction to construct the factor
#' when the factor has two levels.
#' For ptype="prob", the result is a vector of probabilities for the last factor level.
#' For ptype="probmatrix", the result is a matrix  of probabilities for each factor level.
#' @export
#'

CVpredict <- function (fit,newdata,...,ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  UseMethod("CVpredict", fit)
}

# calcPred <- function(ptype,pred=NULL, pthreshold=NULL, ylevels=NULL,ptrans=NULL){
#
#   if (is.null(ptrans) &  is.numeric(pthreshold) & length(ylevels) ==2)
#     ptrans <- function(p){
#       cl <- ifelse(p <= pthreshold, 1, length(ylevels))
#       factor(ylevels[cl], levels=ylevels)
#     }
#
#   if (!is.matrix(pred) & ptype=="pred")
#     return(if (!is.null(ptrans)) ptrans(pred) else pred)
#   if (is.matrix(pred) & ptype=="pred") {
#     if (length(ylevels) > 2){
#       cl <- apply(pred,1, which.max)
#        return(factor(ylevels[cl],levels=ylevels))
#     }
#     # if (!is.null(pthreshold) & ! is.null(ylevels)){
#     #   return(ptrans(pred[,ncol(pred)]))
#     # }
#   }
#   if (is.matrix(pred) & ptype=="prob")
#     return(pred[,ncol(pred)])
#


calcPred <- function(ptype,pred=NULL, pthreshold=NULL, ylevels=NULL,ptrans=NULL){
  
  thresholdfn <- function(p){
    if (is.numeric(pthreshold) & length(ylevels) ==2){
      cl <- ifelse(p <= pthreshold, 1, length(ylevels))
      factor(ylevels[cl], levels=ylevels)
    }
    else p
  }
  
  ans <- pred
  if (ptype == "prob"){
    if (is.matrix(pred)){
      ans <- pred[,ncol(pred)]
    }
    else if (is.factor(pred))
      ans <- as.numeric(pred == tail(ylevels,1))
  } else if (ptype=="pred"){
    if (is.matrix(pred)){
      if (ncol(pred) ==2 && is.numeric(pthreshold))
        ans <- thresholdfn(pred[,2])
      else {
        cl <- apply(pred,1, which.max)
        ans <-factor(ylevels[cl],levels=ylevels)
      }
    }
    else ans <- thresholdfn(ans)
  } else if (ptype=="probmatrix"){
    if (is.factor(pred)){
      ans <- matrix(0, nrow=length(pred), ncol=length(ylevels))
      ans[cbind(1:nrow(ans), as.numeric(pred))]<-1
    }
    else if (is.numeric(pred)){
      if (!is.matrix(pred) || ncol(pred) ==1){
        ans <- cbind(1-pred,pred)
        if (length (ylevels)==2)
          colnames(ans)<- ylevels
      }
    }
  }
  return(if (!is.null(ptrans)) ptrans(ans) else ans)
}




#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.default <- function (fit,newdata,...,ptype="pred",pthreshold=NULL, ylevels=NULL, ptrans=NULL) {
  p <-drop(predict(fit, newdata, ...))
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}



#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.glm <- function (fit, ..., type="response", ptype="pred", pthreshold=NULL, ylevels=NULL, ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(model.frame(fit)[,1])
  #if (length(ylevels) >2) pthreshold <- NULL
  p <- predict(fit, ...,type=type)
  if (fit$family$family == "binomial" & type == "response" & ptype=="pred"){
    if (is.null(pthreshold)) pthreshold <- .5
   }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}


#for binomial family, ptype=pred, returns the predicted class using threshold



#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.lda <- function (fit, ..., ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(model.frame(fit)[,1])
  # if (length(ylevels) >2) pthreshold <- NULL
  p <- predict(fit, ...)
  if (ptype=="pred" & is.null(pthreshold) )
     p <- p$class
  else p <- p$posterior
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}



#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.qda <- function (fit, ..., ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(model.frame(fit)[,1])
  p <- predict(fit, ...)
  if (ptype=="pred" & is.null(pthreshold) )
    p <- p$class
  else p <- p$posterior
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}


#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.nnet <- function (fit, ...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(model.frame(fit)[,1])

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- predict(fit,...,type="raw")[,1]
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    p <- predict(fit,...,type="raw")
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,...,type="class")
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- predict(fit,...,type="raw")
}
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}





#' @describeIn CVpredict  CVpredict method
#' @export


CVpredict.randomForest <- function (fit, ...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(fit$predicted)

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- predict(fit,...,type="response")
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    p <- predict(fit,...,type="prob")
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,...,type="response")
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- predict(fit,...,type="prob")
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}




#' @describeIn CVpredict  CVpredict method
#' @export
#'

CVpredict.rpart <- function (fit, ...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
      ylevels <- attr(fit, "ylevels")

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- predict(fit,...,type="vector")
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    p <- predict(fit,...,type="prob")
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,...,type="class")
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- predict(fit,...,type="prob")
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}


#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.tree <- function (fit, ...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- attr(fit, "ylevels")

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- predict(fit,...,type="vector")
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    p <- predict(fit,...,type="vector")
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,...,type="class")
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- predict(fit,...,type="vector")
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}





#' @describeIn CVpredict  CVpredict method
#' @export


CVpredict.C5.0 <- function (fit, ...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- fit$levels

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    # c5 requires factor response
    p <- NULL
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    p <- predict(fit,...,type="prob")
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,...,type="class")
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- predict(fit,...,type="prob")
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}

#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.svm <- function (fit,...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  #  only gives probs if predict data is provided

  if (is.null(ylevels))
    ylevels <- levels(fitted(fit))

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- predict(fit,...)
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    p <- attr(predict(fit,...,probability=TRUE), "probabilities")
    if (is.null(p)){
      p <- predict(fit,...) 
      pthreshold <- NULL
    }
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,...)
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- attr(predict(fit,...,probability=TRUE), "probabilities")
    if (is.null(p)){
      p1 <- predict(fit,...) 
      p <- matrix(0, nrow=length(p1), ncol=length(ylevels))
      p[,as.numeric(p1)] <- 1
    }
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}






#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.gbm <- function (fit, ...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,
                           n.trees=fit$n.trees,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- fit$classes

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- predict(fit,...,type="response", n.trees=n.trees)
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    p <- predict(fit,...,type="response", n.trees=n.trees)
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,...,type="response", n.trees=n.trees)
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- predict(fit,...,type="response", n.trees=n.trees)
  }
  if (length(dim(p)) ==3) p <- p[,,1]
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}







#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.loess <- function(fit, newdata=NULL, ...){
  if (is.null(newdata))
    predict(fit)
  else predict(fit, as.matrix(newdata[fit$xnames]))
}

#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.ksvm <- function (fit,newdata,...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(kernlab::fitted(fit))

  if (!is.null(newdata))
  newdata <- newdata[, colnames(fit@xmatrix[[ 1]])]

  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- kernlab::predict(fit,newdata,...,type="response")[,1]
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc probmatrix for class prediction using threshold
    if (is.null(fit@prob.model[[1]]))
      p <- kernlab::predict(fit,newdata,...,type="response")
    else p <- kernlab::predict(fit,newdata,...,type="probabilities")
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- kernlab::predict(fit,newdata,...,type="response")
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    if (is.null(fit@prob.model[[1]]))
      p <- kernlab::predict(fit,newdata,...,type="response")
    else p <- kernlab::predict(fit,newdata,...,type="probabilities")
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}



#' @describeIn CVpredict  CVpredict method
#' @export


CVpredict.glmnet <- function(fit,newdata,..., type="response",ptype="pred",pthreshold=NULL,ylevels=NULL,ptrans=NULL,s=NULL,makex=NULL){

  if (!is.matrix(newdata)){
    if (is.function(makex))
      newdata <- makex(newdata)
    else newdata<-model.matrix(~ .-1,data=newdata)
  }
  if (is.null(s)) s <- fit$lambda[1]
  if (inherits(fit, "lognet") && is.null(ylevels)) ylevels <- fit$classnames
  if (inherits(fit, "multnet") && is.null(ylevels)) ylevels <- fit$classnames

  p <- predict(fit,newx=newdata,type=type,s=s,...)
  p <- drop(p)
  if (inherits(fit, "lognet")  && ptype=="pred"){
    if (is.null(pthreshold)) pthreshold <- .5
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}




#' @describeIn CVpredict  CVpredict method
#' @export
#'
CVpredict.cv.glmnet <- function(fit,newdata,..., type="response",ptype="pred",pthreshold=NULL,
                                ylevels=NULL,ptrans=NULL,makex=NULL){

  CVpredict(fit$glmnet.fit,newdata,...,type=type,ptype=ptype,pthreshold=pthreshold,ylevels=ylevels,ptrans=ptrans,
            makex=makex, s=fit$lambda.min)
}


#' @describeIn CVpredict  CVpredict method
#' @export
#'
CVpredict.glmnet.formula <- function(fit,newdata,..., type="response",ptype="pred",pthreshold=NULL,ylevels=NULL,ptrans=NULL,s=NULL){
  if (is.null(ylevels))
    ylevels <- levels(model.frame(fit)[,1])

  if (is.null(s)) s <- fit$lambda[1]
  p <-predict(fit,newdata,type=type,s=s,...)
  p <- drop(p)
  if (inherits(fit, "lognet")  && ptype=="pred"){
    if (is.null(pthreshold)) pthreshold <- .5
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}



#' @describeIn CVpredict  CVpredict method
#' @export
#'
CVpredict.cv.glmnet.formula <- function(fit,newdata,..., type="response",ptype="pred",pthreshold=NULL,ylevels=NULL,ptrans=NULL){
  CVpredict.glmnet.formula(fit,newdata,...,type=type,ptype=ptype,pthreshold=pthreshold,ylevels=ylevels,ptrans=ptrans,
             s=fit$lambda.min)
}


#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.keras.engine.training.Model  <- function(fit, newdata,...,  ptype = "pred", pthreshold = NULL,
                                              ylevels = NULL, ptrans = NULL, batch_size = 32,response=NULL, predictors=NULL){


  if (is.null(ylevels)){
  if (!is.null(response))
     ylevels <- levels(newdata[, response])
  }

  if (!is.null(predictors)) x <- newdata[,predictors] else x <- newdata
   x <- as.matrix(x)


   if (ptype=="pred" && is.null(ylevels)){
     # numeric prediction
     p <- as.numeric(keras::predict_proba(fit,x,  batch_size = batch_size,...))
   }
   else if (ptype=="pred" && is.numeric(pthreshold)){
     # calc probmatrix for class prediction using threshold
     p <- keras::predict_proba(fit,x,  batch_size = batch_size,...)
     if(length(ylevels)==2){
       p <- cbind(1-p,p)
     }
   }
   else if (ptype=="pred"){
     # calc predicted classes
     p <- keras::predict_classes(fit,x,  batch_size = batch_size,...) +1
     p <- ylevels[p]
   }
   else {
     # ptype is "prob" or "probmatrix", calculate probs
     p <- keras::predict_proba(fit,x,  batch_size = batch_size,...)
   }
   calcPred(ptype,p, pthreshold, ylevels,ptrans)
}






hasprobs <- function (model,data=NULL,ylevels = NULL,predictArgs=NULL){

  if (is.null(predictArgs)){
    p <- CVpredict(model, data, ylevels=ylevels,ptype="probmatrix")
  }
  else {
    predictArgs$ptype <- "probmatrix"
     p <- do.call(CVpredict,  c(list(model,data,ylevels=ylevels), predictArgs))
  }
  is.matrix(p)
}



#' @describeIn CVpredict  CVpredict method
#' @export

CVpredict.kde <- function (fit,newdata=fit$x, ..., scale=TRUE) {
  x <- newdata[,fit$names]
  f <- predict(fit, x=x, ...)
  if (scale)
    f <- scaleCondDensity(f, x)
  f
}


scaleCondDensity <- function(f,newd){
  if (length(f) > 1){
    sectiond <- apply(newd, 2, function(x) {
      r <- range(x)
      r[2]-r[1]
    })
    if (all(sectiond != 0))
      return(f)
    else if (any(sectiond != 0)) {
      p <- sum(sectiond !=0)
      n <- round(nrow(newd)^(1/p))
      delta <- prod(sectiond[sectiond!=0])/(n-1)^p
      fac <- delta*sum(f)
      return( f/fac)
    }
  }
  f
}

#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.densityMclust <- function (fit,newdata=NULL,...,ptype="pred",pthreshold=NULL, ylevels=NULL, ptrans=NULL, scale=TRUE) {
  vars <- colnames(fit$data)
  if (!is.null(newdata)) newdata <- newdata[,vars]

  f <- predict(fit, newdata,...)
  if (scale)
    f <- scaleCondDensity(f, newdata)
  f
}

#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.MclustDA <- function (fit, newdata,..., ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(fit$class)
  if (missing(newdata))
    p <- predict(fit, ...)
  else {
    newdata <- newdata[,colnames(fit$data)]
    p <- predict(fit,newdata, ...)
  }
  if (ptype=="pred" & is.null(pthreshold) )
    p <- p$classification
  else p <- p$z
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}


#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.MclustDR <- function (fit, newdata,..., ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- levels(fit$class)
  if (missing(newdata))
    p <- predict(fit, ...)
  else {
    newdata <- newdata[,colnames(fit$x)]
    p <- predict(fit,newdata=newdata, ...)
  }
  if (ptype=="pred" & is.null(pthreshold) )
    p <- p$classification
  else p <- p$z
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}


#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.Mclust <- function (fit, newdata,..., ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- 1:ncol(fit$z)
  if (missing(newdata))
    p <- predict(fit, ...)
  else {
    newdata <- newdata[,colnames(fit$data)]
    p <- predict(fit,newdata, ...)
  }
  if (ptype=="pred" & is.null(pthreshold) )
    p <- p$classification
  else p <- p$z
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}


#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.train <- function(fit,newdata,..., type="response",ptype="pred",pthreshold=NULL,ylevels=NULL,ptrans=NULL){
  
  predvars <- setdiff(names(fit$trainingData), ".outcome")
  newdata <- newdata[,predvars]
  
  if (is.null(ylevels)) ylevels <- levels(fit$trainingData$.outcome)
  
  if (fit$modelType=="Regression"){
    # numeric prediction, same as: if (ptype=="pred" && is.null(ylevels))
    p <- predict(fit, newdata)
  } 
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # fit$modelType=="Classification"
    # calc probmatrix for class prediction using threshold
    p <- as.matrix(predict(fit,newdata,type="prob",...))
  } 
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit, newdata, type = "raw",...)
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    p <- as.matrix(predict(fit,newdata,type="prob",...))
  }
  calcPred(ptype,p, pthreshold, ylevels,ptrans)
}




#' @describeIn CVpredict  CVpredict method
#' @export
CVpredict.bartMachine <- function (fit,newdata,...,type=NULL, ptype="pred",pthreshold=NULL, ylevels=NULL,ptrans=NULL) {
  if (is.null(ylevels))
    ylevels <- fit$y_levels
  newdata <- newdata[,head(colnames(fit$model_matrix_training_data),-1)]
  
  if (ptype=="pred" && is.null(ylevels)){
    # numeric prediction
    p <- predict(fit,newdata,...)
  }
  else if (ptype=="pred" && is.numeric(pthreshold)){
    # calc pred using threshold
    p <- predict(fit,newdata,...,type="class",prob_rule_class=pthreshold)
  }
  else if (ptype=="pred"){
    # calc predicted classes
    p <- predict(fit,newdata,...,type="class")
  }
  else {
    # ptype is "prob" or "probmatrix", calculate probs
    # bart gives prob of first class
    p <- 1-predict(fit,newdata,...,type="prob")
  }
  calcPred(ptype,p, NULL, ylevels,ptrans)
}