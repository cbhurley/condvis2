#' @title Make a list of variable pairings for condition selecting plots
#'
#'
#' @description This function arranges a number of variables in pairs, ordered
#' by their bivariate relationships. The goal is to discover which variable
#' pairings are most helpful in avoiding extrapolations when exploring the data
#' space. Variable pairs with strong bivariate dependencies (not necessarily
#' linear) are chosen first. The bivariate dependency is measured using
#' \code{savingby2d}. Each variable appears in the output only once.
#'
#' @param data A dataframe
#' @param method The character name for the method to use for measuring
#'   bivariate dependency, passed to savingby2d.
#'
#' @return A list containing character vectors giving variable pairings.
#'
#' @details If \code{data} is so big as to make \code{arrangeC} very slow, a
#'   random sample of rows is used instead. The bivariate dependency measures
#'   are rough, and the ordering algorithm is a simple greedy one, so it is not
#'   worth allowing it too much time.
#'
#'
#' @references O'Connell M, Hurley CB and Domijan K (2017). ``Conditional
#'   Visualization for Statistical Models: An Introduction to the
#'   \strong{condvis} Package in R.''\emph{Journal of Statistical Software},
#'   \strong{81}(5), pp. 1-20. <URL:http://dx.doi.org/10.18637/jss.v081.i05>.
#' @export
arrangeC <- function (data, method = "default")
{

  nc.data <- ncol(data)
  if (nc.data <= 2L)
    return(list(colnames(data)))
  data <- na.omit(data)
  nr.data <- nrow(data)
  if (nr.data < 5)
    stop("'data' has less than 5 non-missing rows")

## Calculate a ceiling on the number of rows of 'data' we will use. This is just
## based on a few quick tests on a personal computer, to keep the computation
## time on the order of seconds.

  n <- max(36800 - 6850 * log(nc.data), 100)
  if(nr.data > n)
    data <- data[sample(1:nr.data, n, replace = TRUE), ]

## Construct a matrix of the bivariate dependencies which will be used to order
## the variables

  saving <- matrix(nrow = nc.data, ncol = nc.data)
  colnames(saving) <- rownames(saving) <- colnames(data)
  for (i in 1:nc.data){
    for (j in i:nc.data){
      saving[i, j] <-
      saving[j, i] <- savingby2d(data[, i], data[, j], method)
    }
  }
  diag(saving) <- 1

## Simple greedy ordering of pairs

  C <- list()
  i <- 1L
  while(ncol(saving) > 2){
    pair <- which(saving == min(saving), arr.ind = TRUE)[1L, ]
    C[[i]] <- colnames(saving)[pair]
    saving <- saving[-pair, -pair, drop = FALSE]
    i <- i + 1L
  }
  C[[i]] <- colnames(saving)
  #C below is fix added by CH
  lapply(C, function(y) if (length(y)== 2 & y[1]==y[2]) y[1] else y)

}


savingby2d <- function (x, y = NULL, method = "default")
{
    if(is.data.frame(x) && ncol(x) > 2L) stop("'x' should have max 2 columns.")
    if (is.null(y) && identical(ncol(x), 2L)){
        y <- x[, 2L]
        x <- x[, 1L]
    }
    x <- if (is.data.frame(x))
        x[, 1]
    else x
    y <- if (is.data.frame(y))
        y[, 1]
    else y
    arefactors <- vapply(list(x, y), is.factor, logical(1))
    if (all(arefactors)){
        tab <- table(x, y)
        return(sum(tab != 0) / (ncol(tab) * nrow(tab)))
    } else {
        if (any(arefactors)){
            if (is.factor(x)){
                fac <- x
                cont <- y
            } else {
                fac <- y
                cont <- x
            }
        totalarea <- abs(diff(range(cont)))
        weightbyfac <- table(fac) / length(fac)
        lengthbyfac <- vapply(levels(fac),
            function(x) {
            if (length(cont[as.character(fac) == x]) > 1)
                abs(diff(range(cont[as.character(fac) == x])))
            else 0
            }, numeric(1))
        hullarea <- sum(weightbyfac * lengthbyfac)
        return(hullarea / totalarea)
        } else {
            if (identical(method, "default")){
                if (abs(cor(x, y)) > 0.995) return(0)
                    x.scaled <- (x - mean(x)) / sd(x)
                    y.scaled <- (y - mean(y)) / sd(y)
                    totalarea <- abs(diff(range(x.scaled)) *
                        diff(range(y.scaled)))
                    conhull <- chull(x.scaled, y.scaled)
                    hullarea <- polygonarea(x.scaled[conhull],
                        y.scaled[conhull])
                    return(hullarea / totalarea)
            } else {
                if (method %in% c("Outlying", "Skewed", "Clumpy", "Sparse",
                    "Striated", "Convex", "Skinny", "Stringy", "Monotonic")){
                    if (requireNamespace("scagnostics", quietly = TRUE)){
                        ratio <- scagnostics::scagnostics.default(x, y)[method]
                        if (method %in% c("Outlying", "Skewed", "Clumpy", "Sparse",
                            "Striated", "Skinny", "Stringy", "Monotonic"))
                            ratio <- 1 - ratio
                        return(ratio)
                    } else stop("requires package 'scagnostics'")

                } else {
                    if (identical(method, "DECR")){
                        if (requireNamespace("hdrcde", quietly = TRUE)){
                            o <- hdrcde::hdr.2d(x, y, prob = 0.05)
                            return(sum(o$den$z > o$falpha) / length(o$den$z))
                        } else stop("requires package 'hdrcde'")
                    } else stop("unknown 'method' specified")
                }
            }
        }
    }
}




polygonarea <- function (x, y = NULL)
{
  if (is.null(y) && identical(ncol(x), 2L)){
    y <- x[, 2L]
    x <- x[, 1L]
  }
  area <- 0
  n <- length(x)
  j <- n
  for (i in 1:n){
    area <- area + (x[j] + x[i]) * (y[j] - y[i])
    j <- i
  }
  abs(area) / 2
}


pairoff <- function(vars){
varsx <- vars[seq(1,length(vars),2)]
varsy <- vars[seq(2,length(vars),2)]
if (length(varsx) == length(varsy))
  ans <- mapply(c, varsx,varsy,SIMPLIFY=F,USE.NAMES = F) else {
    ans <- mapply(c, varsx[-length(varsx)],varsy,SIMPLIFY=F,USE.NAMES = F)
    ans[[length(varsx)]]<- varsx[length(varsx)]
  }
ans
}
