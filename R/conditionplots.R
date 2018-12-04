


getCPlotFN <- function(CVdata, var){
  cp <- vector("character",2)
  if (is.numeric(CVdata[[var[1]]])) cp[1]<- "n"else cp[1] <- "f"
  if (length(var)> 1)
    if (is.numeric(CVdata[[var[2]]])) cp[2]<- "n"else cp[2] <- "f"
    CPlotFN <- paste(c("conditionPlot",cp),collapse="")
    # if (type=="gg") CPlotFN <-paste0("gg",CPlotFN)
    if (length(var) > 2) CPlotFN <-"conditionPlotpcp"
    get(CPlotFN)
}


conditionPlot <- function(CVdata, var, varVal, pointColor=NULL,
                          sim=NULL, oldplot=NULL, resetpar=TRUE, plotrows=NULL){

  if (resetpar){
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
  }
  f <- getCPlotFN (CVdata, var)
  f(CVdata, var,varVal, pointColor,sim,plotrows=plotrows)
  return(NULL)
}






conditionPlotn <- function(CVdata, var, varVal,pointColor,sim,oldplot,plotrows){
  # op <- par(no.readonly = TRUE)
  # on.exit(par(op))
  par(mar = c(3, 3, .5,.5),
      mgp = c(1.5, .2, 0),
      tck = -.01)

  h <- hist(CVdata[[var]], xlab=var,col="paleturquoise3",border="grey60",
       breaks = 1.25*nclass.Sturges(1:nrow(CVdata) ),main="")
  varVal <- varVal[[var]]
  if (varVal < min(CVdata[[var]]) | varVal > max(CVdata[[var]]))
    yvarVal <- 0 else {
      b <- which.max(h$breaks > varVal)
      b <- max(1,b-1)
      yvarVal <- h$counts[b]/2
    }
  points(x = varVal, y=yvarVal,col="magenta",pch=43,cex=5)
}




conditionPlotf <- function(CVdata, var, varVal,pointColor,sim,oldplot,plotrows){
  # op <- par(no.readonly = TRUE)
  # on.exit(par(op))
  par(mar = c(3, 3, .5,.5),
      mgp = c(1.5, .2, 0),
      tck = -.01)
  b <- barplot(table(CVdata[[var]]),col="paleturquoise3", border="grey60", width=.8,space=.25, xlab=var)
  var1 <- CVdata[[var]]
  tab <- table(var1)
  varVal <- varVal[[var]]
  crossx <- match(varVal, levels(var1))
  crossy <- tab[crossx]/2
  points(b[crossx,1], crossy, pch=43, col="magenta", cex=5)
}



conditionPlotnn <- function(CVdata, var, varVal,pointColor=NULL,sim,oldplot,plotrows){
  # op <- par(no.readonly = TRUE)
  # on.exit(par(op))
  par(mar = c(3, 3, .5,.5),
      mgp = c(1.5, .2, 0),
      tck = -.01)
  if (!is.null(plotrows)) CVdata <- CVdata[plotrows,]
  if (!is.null(pointColor)) CVdata <- pointColor2var(CVdata,pointColor)
  # size 1.5 is ggplot2 default
  if (is.null(sim))
  plot(CVdata[[var[1]]], CVdata[[var[2]]], col=CVdata[["pointCols"]],
       xlab=var[1], ylab=var[2],pch=20)
  else {
    if (!is.null(plotrows))  sim <- sim[plotrows]
  plot(CVdata[[var[1]]], CVdata[[var[2]]], col=CVdata[["pointCols"]],
       xlab=var[1], ylab=var[2],pch=20)
  CVdatas <- CVdata[sim>0,]
  points(CVdatas[[var[1]]], CVdatas[[var[2]]], col="black")
  }
  points(varVal[[var[1]]],y=varVal[[var[2]]], pch=43, col="magenta",cex=5)

}



conditionPlotnf <- function(CVdata, var, varVal,pointColor,sim,oldplot,plotrows){
  conditionPlotfn(CVdata, rev(var),varVal,pointColor,sim,oldplot,plotrows)
}





conditionPlotfn <- function(CVdata, var, varVal,pointColor,sim,oldplot,plotrows){
  # op <- par(no.readonly = TRUE)
  # on.exit(par(op))
  par(mar = c(3, 3, .5,.5),
      mgp = c(1.5, .2, 0),
      tck = -.01)
  var1 <- CVdata[[var[1]]]
  boxplot(formula(paste0(var[2],"~" ,var[1])), data=CVdata, border="black", col="paleturquoise3", pch=20,
          xlab=var[1], ylab=var[2])
  points(x=match(varVal[[var[1]]], levels(var1)),
               y=varVal[[var[2]]],pch=43, col="magenta",cex=5)
}






conditionPlotff <- function(CVdata, var, varVal,pointColor,sim,oldplot,plotrows){

  par(mar = c(3, 3, .5,.5),
      mgp = c(1.5, .2, 0),
      tck = 0,las=1)

  var2 <- CVdata[[var[2]]]
  var1 <- CVdata[[var[1]]]
  tab <- table(var2, var1)
  barplot(tab, col=scales::hue_pal(h.start=30, l=90)(length(levels(var2))), axes=F, xlab=var[1], ylab=var[2],width=.8,space=.25)
  #axis(1, at=1:length(levels(var1)), labels=levels(var1))
  breaks<- head(seq(0, max(table(var1)),
                  length.out=length(levels(var2))+2),-1)[-1]

  axis(2,at=breaks,labels=levels(var2), lwd=0)

  tab1 <- rbind(0,(tab[-nrow(tab),]))
  mid <- (apply(tab,2,cumsum) + apply(tab1,2,cumsum))/2
  crossx <- match(varVal[[var[1]]], levels(var1))
  crossy <- match(varVal[[var[2]]], levels(var2))
  # print(c(crossx, crossy))
  # print(mid)
   crossy <- mid[crossy,crossx]
  #print(c(crossx, crossy))
 # print(b)
  points(x = crossx-.4,y=crossy, pch=43, col="magenta",cex=5)
  }








conditionClick <- function(CVdata, var,click,plotrows){
   f <- getCClickFN (CVdata, var)
   f(CVdata, var,click,plotrows=plotrows)
}

getCClickFN <- function(CVdata, var){
  cp <- vector("character",2)
  if (is.numeric(CVdata[[var[1]]])) cp[1]<- "n"else cp[1] <- "f"
  if (length(var)> 1)
    if (is.numeric(CVdata[[var[2]]])) cp[2]<- "n"else cp[2] <- "f"
    CPlotFN <- paste(c("conditionClick",cp),collapse="")
    # if (type=="gg") CPlotFN <-paste0("gg",CPlotFN)
    if (length(var) > 2) CPlotFN <-"conditionClickpcp"
    get(CPlotFN)
}

conditionClickn <- function(CVdata, var, click,plotrows){

  setNames(click$x,var)
}



conditionClickf <- function(CVdata, var, click,plotrows){

  rx <- ceiling(click$x)
  var1 <- CVdata[[var]]
  if (rx >= 1 & rx <= length(levels(var1)))
    setNames(levels(var1)[rx],var)
   else NULL
}

conditionClicknn <- function(CVdata, var, click,plotrows){
  setNames(c(click$x,click$y), var)
}





conditionClicknf <- function(CVdata, var, click,plotrows){

  rx <- round(click$x)
  var2 <- CVdata[[var[2]]]
  if (rx >= 1 & rx <= length(levels(var2)))
    setNames( list(click$y,levels(var2)[rx]),var)
  else  NULL
}

conditionClickfn <- function(CVdata, var, click,plotrows){
  rx <- round(click$x)
  var1 <- CVdata[[var[1]]]
  if (rx >= 1 & rx <= length(levels(var1)))
    setNames(list(levels(var1)[rx], click$y),var)
  else  NULL
}



conditionClickff <- function(CVdata, var, click,plotrows){
  var1 <- CVdata[[var[1]]]
  var2 <- CVdata[[var[2]]]
  tab <- table(var2, var1)
  #tab <- tab[nrow(tab):1, ]
  xc <- ceiling(click$x)

  if (xc < 1 | xc > length(levels(var1)))
    return(NULL)
  else {

    tabx <- cumsum(tab[,xc])
    if (click$y > tabx[length(tabx)] | click$y < 0)
      return(NULL)
    else {
      yc <- which.max(tabx > click$y)
      setNames(list(levels(var1)[xc],names(yc)),var)
    }
  }
}

conditionClickpcp <- function(CVdata, var, click,plotrows){
  if (!is.null(plotrows)) CVdata <- CVdata[plotrows,]
  d <- dataprepPCP(CVdata[,var])
  ax <- round(click$y)
  if (abs(ax - click$y) <= .1){
    row <- which.min(abs(d[,ax] - click$x))
    res <- CVdata[row,var]
  }
  else res <-NULL
  res
}


dataprepPCP <- function(x){
  for ( i in 1:ncol(x)){
    if (is.factor(x[[i]]))
      x[[i]]<- as.numeric(x[[i]])
  }
  nums <- sapply(x, is.numeric)
  x <- as.matrix(x[nums])
  x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/
               (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  x
}
conditionPlotpcp <- function(CVdata, var,varVal, pointColor,sim,plotrows){

  par(mar = c(1, 3, .5,.5),
      mgp = c(1.5, .2, 0),
      tck = -.01)
  if (!is.null(pointColor)) CVdata <- pointColor2var(CVdata,pointColor)
  if (!is.null(plotrows)){
    CVdata <- CVdata[plotrows,]
    if (!is.null(sim)) sim <- sim[plotrows]
  }

  col <- CVdata[["pointCols"]]
  lwd <- rep(.1, length(col))
  col <- c(col, "magenta")
  lwd <- c(lwd,3)

  CVdata1 <- rbind(CVdata[,var], varVal[,var])
  if (!is.null(sim)) {
    sim <- c(sim,1)
    lwd[sim> 0] <- 2
  }
  parcoord1(dataprepPCP(CVdata1), col = col, lwd=lwd, horiz=FALSE, autoscale=FALSE)

}
