


#' Plots diagnostics for the tour supplied
#'
#' @param path the tour
#' @param data the dataset
#' @param pathlen the pathlength
#' @param threshold used for similarityweight
#' @param which subset of 1:3
#' @param ... other args for similarityweight
#'
#' @return Table of max sims attained.
#' @export
#' @details The first plot shows approximately how much data are visible on each section,  the second shows what proportion of data are \emph{visited} by the tour, and the third a density estimate of max similarity values.


plotTourDiagnostics <- function(path, data, pathlen=nrow(path),threshold=1,which=1:3,...){
  conditionvars <- names(path)

  sim <- similarityweight(path,data[conditionvars], threshold=threshold,...)
  simmax <- apply(sim,2,max)
  par(mfrow=c(1,length(which)))
  if (1 %in% which){
  rsk <- rowSums(sim) / ncol(sim)
  index <- seq(from=1, to=pathlen, length.out = length(rsk))
  plot(index, rsk, type = "l", xlab = "Path index", ylab = "sum of sim/n",main="")
  w <- round(seq(from=1, to=length(rsk), length.out = pathlen))
  points(index[w],rsk[w], pch=16, col="grey")
  }
  if (2 %in% which){
  
  plot(ecdf(simmax), verticals=T, main="", pch=NA, xlab =
         "max sim per obs",ylab="proportion of data", xlim=c(0,1))
  grid()
  }

  if (3 %in% which){
      plot(density(simmax, from=0, to=1),  xlab="max sim per obs", main="")
  }
  par(mfrow=c(1,1))
  tab <- table(cut(simmax, (0:5)/5))
  tab <- c(length(simmax) - sum(tab), tab)
  names(tab)[1] <- "0"
  tab
}
