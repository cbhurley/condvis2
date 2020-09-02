#' Constructs a various tours of data space
#' 
#' @name tours
#' @param data A dataframe
#' @param length Path length, defaults to 10
#' @param reorder If TRUE (default) uses DendSer to reorder the path \code{\link[DendSer]{dser}}
#' @param conditionvars A vector of variable names. The returned tour is for this subset of variables.
#' @param var A variable name for alongPath
#' @param current Default value of variables for alongPath
#' @param ... ignored
#' @return A dataframe with the path
#' @examples
#' randomPath(mtcars,length=5)
#' seqPath(mtcars,length=5)
#' alongPath(mtcars,var="mpg", length=5, current=mtcars[1,])

NULL

#' @describeIn tours Constructs a tour of data space following random observations
#' @export
#' 
randomPath<- function(data, length=10, reorder=TRUE,conditionvars=NULL,...){

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
    d <- cluster::daisy(rpath, stand=TRUE, warnType=FALSE)
    o <- DendSer::dser(d)
    rpath <- rpath[o,]
    }
  }
  rpath
  }



#' @describeIn tours Constructs a tour of data space following first length observations
#' @export
#' 
seqPath<- function(data, length=10, reorder=FALSE,conditionvars=NULL,...){
  
  if (length(conditionvars)==0) conditionvars <- NULL
  if (length > nrow(data)) {
    warning(paste("Cannot compute path of length", length))
    return(NULL)
  }
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]
  s <- 1:length
  rpath<- data[s,,drop=FALSE]
  if (reorder){
    if (ncol(rpath) ==1)
      rpath <- rpath[order(rpath[,1]),,drop=FALSE]
    else{
      d <- cluster::daisy(rpath, stand=TRUE, warnType=FALSE)
      o <- DendSer::dser(d)
      rpath <- rpath[o,]
    }
  }
  rpath
}


#' @describeIn tours Constructs a tour of data space  of length equi-spaced values in the range of var.
#'  If var is a factor, its levels are used.
#' @export
#' 
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










scorePath<- function(data, score,length=10, reorder=TRUE,conditionvars=NULL){
  if (length(conditionvars)==0) conditionvars <- NULL
  # find rows with the highest score values
  q <- sort(score,decreasing=T)[length]
  s <-which(score >= q)
  if (!is.null(conditionvars)) data <- data[,conditionvars,drop=FALSE]

  lpath<- data[s,,drop=F]
  if (reorder){
    d <- cluster::daisy(lpath, stand=TRUE, warnType=FALSE)
    o <- DendSer::dser(d)
    lpath <- lpath[o,,drop=F]
  }
  structure(lpath, rows = s[o])
}





pathInterpolate <-function (x, ninterp=4){
  if (ninterp < 0)
    stop("'ninterp' should be >= 0")
  if (ninterp==0) x
  else UseMethod("pathInterpolate", x)
  }


pathInterpolate.default <- function (x, ninterp = 4L){
     x <- as.numeric(x)
    xdiff <- diff(x) / (ninterp + 1L)
    #add <- matrix(xdiff, nrow = ninterp + 1L, ncol = length(xdiff), byrow = TRUE)
    cumsum(c(x[1L], rep(xdiff, each = ninterp + 1L)))
  }



pathInterpolate.factor <- pathInterpolate.character <-function (x, ninterp = 4L){
       # if (!identical(ninterp %% 2, 0))
    #   warning("'ninterp' should be even for factor/character vector")
    factor(unlist(list(rep(head(x, 1L), 1L + floor(ninterp / 2L)),
                rep(head(tail(x, -1L), -1L), each = ninterp + 1L),
                rep(tail(x, 1L), 1L + ceiling(ninterp / 2L)))))
  }




pathInterpolate.data.frame <- function(x, ninterp = 4L){
  ans <- lapply(x, pathInterpolate, ninterp)
  ans <- data.frame(ans)
  s <- (1:nrow(x))*(ninterp+1) -1
  # avoid rounding error
  ans[s,]<- x 
  ans
}








  