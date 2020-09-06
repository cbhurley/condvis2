
#' Constructs tours of data space based on fits.
#' 
#' @name fitPath
#' @param data A dataframe
#' @param fits A model fit or list of fits
#' @param length Path length, defaults to 10
#' @param reorder If TRUE (default) uses DendSer to reorder the path \code{\link[DendSer]{dser}}
#' @param conditionvars A vector of variable names. The returned tour is for this subset of variables.
#' @param predictArgs Extra inputs to CVpredict
#' @param response The name of the response variable
#' @param ... ignored
#' @return A dataframe with the path
#' @examples
#' fit1 <- lm(mpg ~ wt+hp+am, data=mtcars)
#' fit2 <- lm(mpg ~ wt, data=mtcars)
#' lofPath(mtcars,fit1, response="mpg")
#' diffitsPath(mtcars,list(fit1,fit2))

NULL



#' @describeIn fitPath Constructs a tour of data space showing biggest absolute residuals from fits.
#' @export

lofPath<- function(data, fits,length=10, reorder=TRUE,conditionvars=NULL,
                   predictArgs=NULL,response=NULL,...){
  
  if (!inherits(fits, "list")) fits <- list(fits)
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }
  if (is.null(response)) return(NULL)
  y <- data[[response]]
  
  # y <- matrix(y, ncol=length(fits), nrow=length(y))
  f <- vector("list",length=length(fits))
  
  if (length(predictArgs) == length(fits)){
    for (i  in 1:length(fits)){
      pargs <- predictArgs[[i]]
      if (!is.null(pargs$response))
      f[[i]] <- do.call(CVpredict,  c(list(fits[[i]],data, ptype="pred"), pargs))
      else
        f[[i]] <- do.call(CVpredict,  c(list(fits[[i]],data, ptype="pred", response=response), pargs))
    }
  } else {
    for (i  in 1:length(fits)){
      f[[i]] <- CVpredict(fits[[i]],data, ptype="pred", response=response)
    }
  }
  
  w <- sapply(f, is.numeric)
  facs <- sapply(f, is.factor)
  if (is.numeric(y) && sum(w)>= 1) {
    f <- simplify2array(f[w])
    rall <- abs(f - as.numeric(y))  
    r <- apply(rall,1,max)
    q <- sort(r,decreasing=T)[length]
    s <- which(r >= q)[1:length]
    
  }
  else if (is.factor(y) && sum(facs)>= 1) {
    f <- simplify2array(f[facs])
    dif <- sapply(1:length(y), function(i) sum(y[i] != f[i,]))
    length <- min(length, sum(dif>0))
    q <- sort(dif,decreasing=T)[length]
    s <-which(dif >= q & dif > 0)[1:length]
    
  }
  
  if (is.na(s[1])) return(NULL)
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  
  lpath<- data[s,,drop=F]

  if (reorder & nrow(lpath)> 2){
    d <- cluster::daisy(lpath,warnType=FALSE)
    o <- DendSer::dser(d)
    lpath <- lpath[o,]
    structure(lpath, rows = s[o])
  } else
  structure(lpath, rows = s)
  
}



#' @describeIn fitPath Constructs a tour of data space showing biggest differences in fits.
#' @export
diffitsPath<- function(data, fits,length=10, reorder=TRUE,conditionvars=NULL,predictArgs=NULL,...){
  if (!inherits(fits, "list")) fits <- list(fits)
  if (length(fits) <2) {
    warning("Provide two or more fits")
    return(NULL)
  }
  
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }

  f <- vector("list",length=length(fits))
  if (length(predictArgs) == length(fits)){
    for (i  in 1:length(fits)){
      f[[i]] <- do.call(CVpredict,  c(list(fits[[i]],data, ptype="pred"), predictArgs[[i]]))
    }
  } else {
    for (i  in 1:length(fits)){
      f[[i]] <- CVpredict(fits[[i]],data, ptype="pred")
    }
  }

  w <- sapply(f, is.numeric)
  facs <- sapply(f, is.factor)
  if (sum(w)>= 2) {
  f <- simplify2array(f[w])
  
  dif <- apply(f,1,max)- apply(f,1,min)
  length <- min(length, sum(dif>0))
  q <- sort(dif,decreasing=T)[length]
  s <-which(dif >= q)
  }
  else if (sum(facs)>= 2) {
    f <- simplify2array(f[facs])
    dif <- apply(f,1, function(x) length(unique(x)))
    length <- min(length, sum(dif>1))
    q <- sort(dif,decreasing=T)[length]
    s <-which(dif >= q & dif > 1)
  }
  else {
    warning("Cannot calculate differences")
    return(NULL)
  }
 
  if (is.na(s[1])) return(NULL)
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  
  if (length(s) > length)
    s <- s[1:length]
  lpath<- data[s,,drop=F]
 
  if (reorder & nrow(lpath)> 2){
    d <- cluster::daisy(lpath, warnType=FALSE)
    o <- DendSer::dser(d)
    lpath <- lpath[o,,drop=F]
  }
  else o <- 1:nrow(lpath)
  structure(lpath, rows = s[o])
  
}


#' @describeIn fitPath Constructs a tour of data space showing high  (numeric) response values
#' @export
hiresponsePath<- function(data, response=NULL, length=10, reorder=TRUE,
                          conditionvars=NULL,...){
  if (length(conditionvars)==0) conditionvars <- NULL
  score <- data[[response]]
  if (!is.numeric(score)) {
    warning("Response is not numeric")
    return(NULL)
  }
  scorePath(data, score,length=length, reorder=reorder,conditionvars=conditionvars)
}


#' @describeIn fitPath Constructs a tour of data space showing low  (numeric) response values
#' @export
loresponsePath<- function(data, response=NULL, length=10, reorder=TRUE,
                          conditionvars=NULL,...){
  if (length(conditionvars)==0) conditionvars <- NULL
  score <- -data[[response]]
  if (!is.numeric(score)) {
    warning("Response is not numeric")
    return(NULL)
  }
  scorePath(data, score,length=length, reorder=reorder,conditionvars=conditionvars)
}

