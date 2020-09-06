
#' Constructs  tours of data space based on centers of clusters
#' 
#' @name clusPath
#' @param data A dataframe
#' @param length Path length, defaults to 10
#' @param reorder If TRUE  uses DendSer to reorder the path \code{\link[DendSer]{dser}}
#' @param conditionvars A vector of variable names. The returned tour is for this subset of variables.
#' @param maxn (pamPath only) For datasets with more than maxn rows, use maxn randomly selected rows.
#' @param cl A vector specifying cluster membership for rows of data.
#' @param ... ignored
#' @return A dataframe with the path
#' @importFrom cluster  daisy pam clara
#' @importFrom DendSer  dser
#' @examples
#' kmeansPath(mtcars,length=4)
#' pamPath(mtcars,length=4)
#' claraPath(mtcars,length=4)
#' medoidPath(mtcars,cl=rep(1:3, length.out=nrow(mtcars)))

NULL


#' @describeIn clusPath Constructs a tour of data space following  length k-means centroids
#' @export
kmeansPath<- function(data, length=10, reorder=TRUE,conditionvars=NULL,...){
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

#' @describeIn clusPath  Constructs a tour of data space following  length \code{\link[cluster]{pam}} medoids
#' @export
pamPath<- function(data, length=10, reorder=TRUE,conditionvars=NULL,maxn=4000,...){
  if (is.numeric(maxn) && nrow(data) > maxn){
    data <- data[sample(nrow(data), maxn),]
  }
  if (length(conditionvars)==0) conditionvars <- NULL
  if (length > nrow(data)) {
    warning("Pick length <= nrows")
    return(NULL)
  }
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  if (nrow(data) >= 5000) {
    print("Calculating Kmed path...")
    do.swap <- FALSE}
  else do.swap <- TRUE
  
  d <- cluster::daisy(data,stand=TRUE, warnType=FALSE)
  clustering <- cluster::pam(d, k = length,pamonce=5, do.swap=do.swap,
                             keep.diss=FALSE, keep.data=FALSE)
  centers <- data[clustering$medoids, ,drop=F]
 
  if (reorder){
    d <- cluster::daisy(centers,stand=TRUE, warnType=FALSE)
    o <- DendSer::dser(d)
    centers <- centers[o,,drop=F]
  }
  if (nrow(data) >= 5000) print("Kmed path calculated")
  centers
}

#' @describeIn clusPath  Constructs a tour of data space following  length \code{\link[cluster]{clara}} medoids
#' @export
claraPath<- function(data, length=10, reorder=TRUE,conditionvars=NULL,...){
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
  
  if (reorder){
    d <- dist(scale(centers))
    o <- DendSer::dser(d)
    centers <- centers[o,,drop=F]
  }
  data.frame(centers)
}

#' @describeIn clusPath Returns a path visiting cluster medoids
#' @export

medoidPath<- function(data, cl,reorder=FALSE){
  clu <- unique(cl)
  centers <- data[1:length(clu),]
  for (i in clu){
    r <- which(cl==i)
    m <-medoid(data[r,])
    centers[i,] <-as.list(m)
  }
  if (reorder){
    d <- cluster::daisy(centers,stand=TRUE,warnType=FALSE)
    o <- DendSer::dser(d)
    centers <- centers[o,,drop=F]
  }
  centers
}
# medoidPath<- function(data, cl,reorder=FALSE){
#   clu <- unique(cl)
#   d <- as.matrix(cluster::daisy(data, stand=TRUE))
#   rows <- sapply(clu, function(i){ r <- which(cl==i)
#   m <- which.min(colMeans(d[r,r,drop=FALSE]))
#   r[m]
#   })
#   centers <- data[rows,,drop=FALSE]
#   if (reorder){
#     d <- cluster::daisy(centers,stand=TRUE)
#     o <- DendSer::dser(d)
#     centers <- centers[o,,drop=F]
#   }
#   centers
# }


#' @describeIn clusPath Returns a path visiting cluster centroids
#' @export

centroidPath<- function(data, cl,reorder=FALSE){
  centers <- aggregate(data,list(cl),mean)[,-1]
  if (reorder){
    d <- cluster::daisy(centers,stand=TRUE, warnType=FALSE)
    o <- DendSer::dser(d)
    centers <- centers[o,,drop=F]
  }
  centers
}


#' Finds medoid of data
#' 
#' @param data A dataframe
#' @param maxn For datasets with more than maxn rows, use maxn randomly selected rows.
#' @return A dataframe with one row, which is the medoid of the data, based on (standardised) daisy dist
#' @export
#' @examples
#' medoid(mtcars)
#' 

medoid<- function(data, maxn = 4000) {
  if (is.numeric(maxn) && nrow(data) > maxn){
    data <- data[sample(nrow(data), maxn),]
  }
  ctype <- which(sapply(data, function(v) !is.numeric(v) & ! is.factor(v)))
  for (ct in ctype) data[[ct]] <- as.factor(data[[ct]])
  
  d <- as.matrix(cluster::daisy(data,stand=TRUE, warnType=FALSE))
  w <- which.min(colMeans(d))
  data[w, ,drop=F]
}
