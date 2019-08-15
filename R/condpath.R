
#' Tours of data space
#' @param data A dataset
#' @param fits A model fit or list of fits
#' @param length The length of path returns
#' @param reorder If TRUE, points on the path are re-ordered so nearby points are close in the path
#' @param score A vector of length equal to the nrows of data.
#' @param conditionvars A vector of variable names. Some tours will limit calculation to this subset of variables.
#' @param predictArgs a list with one entry per fit, giving arguments for CVpredict
#' @param ...  other arguments, ignored
#' @name condtour
#' @return A dataframe, which is the path
#' @details kmeansPath works for both numeric and factors, which are converted to columns of indicators.
#' pamPath is not recommended for large datasets, use claraPath instead.
#' fits are used only in lofPath and diffitsPath.
#' Paths are reordered using dser from package DendSer.
#' @importFrom cluster  daisy pam clara
#' @importFrom DendSer  dser
#' @importFrom kmed  fastkmed

NULL


#' @describeIn condtour Returns a random path
#' @export
randomPath<- function(data, fits=NULL,length=10, reorder=TRUE,conditionvars=NULL,...){

  if (length(conditionvars)==0) conditionvars <- NULL
  if (length > nrow(data)) {
    warning(paste("Cannot compute path of length", length))
    return(NULL)
  }
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  s <- sample(nrow(data),length)
  rpath<- data[s,,drop=FALSE]

  if (reorder){
    if (ncol(rpath) ==1)
      rpath <- rpath[order(rpath[,1]),,drop=FALSE]
    else{
    d <- cluster::daisy(rpath, stand=TRUE)
    o <- DendSer::dser(d)
    rpath <- rpath[o,]
    }
  }
  rpath
  }


#' @describeIn condtour Returns a  path along var
#' @export
alongPath<- function(data, var,length=10,current=NULL,...){
  
  dv <- data[[var]]

  if (is.numeric(dv))
   new <- seq(min(dv), max(dv), length.out=length)
  else {
    dv <- as.factor(dv)
    new<- levels(dv)
  }
  if (!is.null(current)){
    current[[var]]<- NULL
    current<- data.frame(new, current,row.names=NULL)
    names(current)[1]<- var
    return(current)
  }
  else return (data.frame(var=new))
}

expandPath<- function(path,current=NULL){
  if (is.null(path)) return(NULL)
  res <- path
  if (!is.null(current)){
    borrow <- setdiff(names(current), names(path))
    if (length(borrow)>=1)
    res<- data.frame(path, current[borrow],row.names=NULL)
  }
  return(res)
}

#'@describeIn condtour Returns a path using kmeans centroids
#'@export
kmeansPath<- function(data,fits=NULL, length=10, reorder=TRUE,conditionvars=NULL,...){
  if (length(conditionvars)==0) conditionvars <- NULL
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  n <- nrow(data)
  nfac <-sapply(data, is.factor)
  # if (sum(nfac) == n){
  #   print("cannot calculate kmeans path")
  #   return(NULL)
  # }

  if (sum(nfac)==0) datan <- data else datan <- data[,!nfac,drop=FALSE]

  if (sum(nfac) < ncol(data)){
  x <- scale(datan)

  means <- attr(x, "scaled:center")
  sds <- attr(x, "scaled:scale")
  } else {
    x <- NULL
    datan <- NULL
  }

  if (sum(nfac) >0){
  facs <- names(data)[nfac]
  dummylist<- vector("list", length(facs))

  for (i in seq(along=facs)){
    z <- data[[facs[i]]]
    levs <- levels(z)
    res <- matrix(0, nrow=length(z),ncol=length(levs))
    res[cbind(seq(along=z),match(z, levs))]<- 1
    dummylist[[i]] <- res
  }
  dummy <- do.call(cbind,dummylist)
    x <- cbind(x, dummy)
  }
  
  clustering <- kmeans(x, centers = length)
  centers <- clustering$centers
 
  if (reorder & nrow(centers)> 2){
    d <- dist(centers)
    o <- DendSer::dser(d)
    centers <- centers[o,,  drop = FALSE]
  }
  if (sum(nfac) < ncol(data)){
  centersn <- centers[,1:ncol(datan), drop=FALSE]
  centersn <- data.frame(sapply(seq(along=means), function(i) centersn[,i]*sds[i]+ means[i]))
  names(centersn)<- names(datan)
  result <- centersn
  } else result <- NULL

   # if (sum(nfac)>0){
   #   s <- sample(nrow(data),length)
   #   rpath<- data[s, nfac]
   #
   #   centers <- cbind(centers, rpath)
   # }

  if (sum(nfac) >0){
    if (sum(nfac) == ncol(data))
      centersf <- centers else
        centersf <- centers[,-(1:ncol(datan))]
    w <- cumsum(c(1,sapply(dummylist, ncol)))
     facc <- lapply(seq(along=w[-1]), function(i) {
      z <- centersf[, w[i]: (w[i+1]-1)]
      factor(levels(data[[facs[i]]])[apply(z,1, which.max)])
    })
    facc<- data.frame(facc)
    names(facc)<- facs
    if (!is.null(result)) result <- cbind(result, facc) else result <- facc
  }
  result
}

#' @describeIn condtour Returns a path using pam medoids from package cluster
#' @export

pamPath<- function(data, fits=NULL,length=10, reorder=TRUE,conditionvars=NULL,...){
  if (length(conditionvars)==0) conditionvars <- NULL
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  if (nrow(data) >= 5000) print("Calculating Kmed path...")
  
  d <- cluster::daisy(data,stand=TRUE)
  clustering <- cluster::pam(d, k = length,pamonce=5)
  centers <- data[clustering$medoids, ,drop=F]
 
  if (reorder){
    d <- cluster::daisy(centers,stand=TRUE)
    o <- DendSer::dser(d)
    centers <- centers[o,,drop=F]
  }
  if (nrow(data) >= 5000) print("Kmed path calculated")
  centers
}

#' @describeIn condtour Returns a path using clara medoids from package cluster
#' @export

claraPath<- function(data, fits=NULL,length=10, reorder=TRUE,conditionvars=NULL,...){
  if (length(conditionvars)==0) conditionvars <- NULL
  # data must be numeric
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }

  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  nnum <-sapply(data, is.numeric)
  if (!all(nnum)) {
    warning("All variables must be numeric for clara")
    return(NULL)
  }
  clustering <- cluster::clara(data, k = length, stand=TRUE)
  centers <- clustering$medoids
  print(centers)
  if (reorder){
    d <- dist(scale(centers))
    o <- DendSer::dser(d)
    centers <- centers[o,,drop=F]
  }
  data.frame(centers)
}


#' @describeIn condtour Returns a path using fastkmed from package kmed
#' @export

fastkmedPath<- function(data, fits=NULL,length=10, reorder=TRUE,conditionvars=NULL,...){
  if (length(conditionvars)==0) conditionvars <- NULL
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  if (nrow(data) >= 5000) print("Calculating Kmed path...")
  
  d <- cluster::daisy(data,stand=TRUE)
  class(d)<- "dist"
  
  clustering <- kmed::fastkmed(d, ncluster = length, iterate=50)
  centers <- data[clustering$medoid, ,drop=FALSE]

  if (reorder){
    if (ncol(centers) ==1)
      centers <- centers[order(centers[,1]),,drop=FALSE]
    else{
      d <- cluster::daisy(centers,stand=TRUE)
      o <- DendSer::dser(d)
      centers <- centers[o,,drop=F]
    }
  }
  if (nrow(data) >= 5000) print("Kmed path calculated")
  centers
}

#'@describeIn condtour Returns a path showing biggest absolute residuals from fits.
#'@export

lofPath<- function(data, fits,length=10, reorder=TRUE,conditionvars=NULL,predictArgs=NULL,response=NULL,...){
  
  if (!inherits(fits, "list")) fits <- list(fits)
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }
  if (is.null(response)) return(NULL)
   y <- data[[response]]
   if (!is.numeric(y)) return(NULL)
  if (length(predictArgs) == length(fits)) {
    f <- vector("list",length=length(fits))
    for (i  in 1:length(fits)){
      f[[i]] <- do.call(CVpredict,  c(list(fits[[i]],data), predictArgs[[i]]))
    }
  }
  else f <- lapply(fits, CVpredict,data)
  
  w <- sapply(f, is.numeric)
  
  if (sum(w)>= 1) {
    f <- simplify2array(f[w])
    rall <- abs(f - y)
  r <- apply(rall,1,max)
  q <- sort(r,decreasing=T)[length]
  s <- which(r >= q)[1:length]
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
 
  lpath<- data[s,,drop=F]
  if (reorder){
    d <- cluster::daisy(lpath)
    o <- DendSer::dser(d)
    lpath <- lpath[o,]
  }
  structure(lpath, rows = s[o])
  }
}

# lofPath<- function(data, fits,length=10, reorder=TRUE,conditionvars=NULL,predictArgs=NULL){
#   # this one should be fixed to work with CVpredict and response for data
#   # removed from ui for the moment
#   if (!inherits(fits, "list")) fits <- list(fits)
#   if (length > nrow(data)) {
#     warning("Pick length <= nrows")
#     return(NULL)
#   }
#   
#   if (!is.null(predictArgs))
#     warning("predictArgs are ignored")
#   
#   rall <- lapply(fits, residuals)
#   w <- !sapply(rall, is.null)
#   if (sum(w) != 0) rall <- simplify2array(rall[w])
#   else {
#     warning("No residuals defined for fits")
#     return(NULL)
#   }
#   if (! is.matrix(rall)){
#     warning("Residual vectors have different lengths")
#     return(NULL)
#   }
#   if (sum(w) != length(fits))
#     warning("Residuals not defined for some fits")
#   rall <- abs(rall)
#   r <- apply(rall,1,max)
#   q <- sort(r,decreasing=T)[length]
#   s <- which(r >= q)[1:length]
#   if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
#   
#   lpath<- data[s,,drop=F]
#   if (reorder){
#     d <- cluster::daisy(lpath)
#     o <- DendSer::dser(d)
#     lpath <- lpath[o,]
#   }
#   structure(lpath, rows = s[o])
# }


#'@describeIn condtour Returns a path showing biggest difference in fits
#'@export

diffitsPath<- function(data, fits,length=10, reorder=TRUE,conditionvars=NULL,predictArgs=NULL,...){
  if (!inherits(fits, "list")) fits <- list(fits)
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }

  if (length(predictArgs) == length(fits)) {
    f <- vector("list",length=length(fits))
    for (i  in 1:length(fits)){
      f[[i]] <- do.call(CVpredict,  c(list(fits[[i]],data), predictArgs[[i]]))
    }
  }
  else f <- lapply(fits, CVpredict,data)

  w <- sapply(f, is.numeric)
  facs <- sapply(f, is.factor)
  if (sum(w)>= 2) {
  f <- simplify2array(f[w])
  
  dif <- apply(f,1,max)- apply(f,1,min)
  q <- sort(dif,decreasing=T)[length]
  s <-which(dif >= q)
  }
  else if (sum(facs)>= 2) {
    f <- simplify2array(f[facs])
    dif <- apply(f,1, function(x) length(unique(x)))
    q <- sort(dif,decreasing=T)[length]
    s <-which(dif >= q & dif > 1)
  }
  else {
    warning("Cannot calculate differences")
    return(NULL)
  }
  
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  
  if (length(s) > length)
    s <- s[1:length]
  lpath<- data[s,,drop=F]
 
  if (reorder & nrow(lpath)> 2){
    d <- cluster::daisy(lpath)
    o <- DendSer::dser(d)
    lpath <- lpath[o,,drop=F]
  }
  else o <- 1:nrow(lpath)
  structure(lpath, rows = s[o])
  
}

#' @describeIn condtour Returns a path showing highest scores
#' @export

createPath<- function(data, score,length=10, reorder=TRUE,conditionvars=NULL){
  if (length(conditionvars)==0) conditionvars <- NULL
  # find rows with the highest score values
  q <- sort(score,decreasing=T)[length]
  s <-which(score >= q)
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]

  lpath<- data[s,,drop=F]
  if (reorder){
    d <- cluster::daisy(lpath, stand=TRUE)
    o <- DendSer::dser(d)
    lpath <- lpath[o,,drop=F]
  }
  structure(lpath, rows = s[o])
}

#' Interpolation
#'
#' @param x a numeric or factor vector or dataframe
#' @param ninterp number of interpolated steps
#' @return interpolated version of x
#' @export


pathInterpolate <-function (x, ninterp=4){
  if (ninterp < 0)
    stop("'ninterp' should be >= 0")
  if (ninterp==0) x
  else UseMethod("pathInterpolate", x)
  }

#' @describeIn pathInterpolate Default interpolate method
#' @export

pathInterpolate.default <- function (x, ninterp = 4L){
     x <- as.numeric(x)
    xdiff <- diff(x) / (ninterp + 1L)
    #add <- matrix(xdiff, nrow = ninterp + 1L, ncol = length(xdiff), byrow = TRUE)
    cumsum(c(x[1L], rep(xdiff, each = ninterp + 1L)))
  }

## Method dispatch does not seem to be working for factor/character.



##' @describeIn pathInterpolate  pathInterpolate method for factor
##' @export

pathInterpolate.factor <- pathInterpolate.character <-function (x, ninterp = 4L){
       # if (!identical(ninterp %% 2, 0))
    #   warning("'ninterp' should be even for factor/character vector")
    factor(unlist(list(rep(head(x, 1L), 1L + floor(ninterp / 2L)),
                rep(head(tail(x, -1L), -1L), each = ninterp + 1L),
                rep(tail(x, 1L), 1L + ceiling(ninterp / 2L)))))
  }


#' @describeIn pathInterpolate  pathInterpolate method for data.frame
#' @export

pathInterpolate.data.frame <- function(x, ninterp = 4L){
  ans <- lapply(x, pathInterpolate, ninterp)
  ans <- data.frame(ans)
  s <- (1:nrow(x))*(ninterp+1) -1
  # avoid rounding error
  ans[s,]<- x 
  ans
}

