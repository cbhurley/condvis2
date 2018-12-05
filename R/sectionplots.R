

#' Plots the main condvis display
#' @param CVdata the dataset used for the fit
#' @param CVfit a fit or list of fits
#' @param response name of response variable
#' @param preds names of predictors
#' @param sectionvar section variable
#' @param conditionvals conditioning values. A list or dataframe with one row
#' @param pointColor a color, or the name of variable to be used for coloring
#' @param sim vector of similarity weights
#'@param threshold used for similarity weights, defaults to 1.
#' @param linecols vector of line colours
#' @param dataplot if CVfit is NULL, the data are plotted using this function. Defaults to a parallel coordinate plot
#'@param gridsize used to contruct grid of fitted values.
#'@param probs Logical; if \code{TRUE}, shows predicted class probabilities instead of just predicted classes. Only available with two numeric sectionvars and the model's predict method provides this.
#' @param view3d Logical; if \code{TRUE} plots a three-dimensional regression surface if possible.
#' @param theta3d,phi3d Angles defining the viewing direction. \code{theta3d} gives the azimuthal direction and \code{phi3d} the colatitude. See\code{\link[graphics]{persp}}.
#'@param xlim passed on to plot
#'@param ylim passed on to plot
#'@param zlim passed on to plot
#'@param pointSize used for points
#'@param predictArgs a list with one entry per fit, giving arguments for predict
#'@param resetpar When TRUE (the default) resets pars after drawing.
#'@param density default FALSE. Use TRUE if model is a density function.
#'@param showdata  If FALSE, data on section not shown.
#'@param returnCoords  If TRUE, returns coordinates for some plots
#' @return plotted coordinates, for some plots
#' @export
#'
sectionPlot <- function(CVdata, CVfit,response,preds,sectionvar,conditionvals,pointColor=NULL,
                        sim=NULL,threshold=1,linecols=NULL,
                        dataplot=NULL, gridsize=50, probs=FALSE, view3d=FALSE,
                        theta3d = 45, phi3d = 20, xlim=NULL,ylim=NULL, zlim=NULL,pointSize=2,
                        predictArgs=NULL, resetpar=TRUE, density=FALSE, showdata=density==FALSE,
                        returnCoords=FALSE){

  op <- par(no.readonly=TRUE)
  if (resetpar) on.exit(par(op))
  if (view3d) gridsize <- 20


  conditionvars <-setdiff(preds, sectionvar)

  conditionvals <- as.data.frame(conditionvals)

  if (!is.null(pointColor)) CVdata <- pointColor2var(CVdata,pointColor)

  sp <- vector("character",3)
  if (is.numeric(CVdata[[response]])) sp[1] <- "n" else sp[1] <- "f"
  if (is.numeric(CVdata[[sectionvar[1]]])) sp[2]<- "n"else sp[2] <- "f"
  if (length(sectionvar)> 1)
    if (is.numeric(CVdata[[sectionvar[2]]])) sp[3]<- "n"else sp[3] <- "f"
  sp <- paste(sp,collapse="")
  sectionPlotFN <- get(paste(c("sectionPlot",sp),collapse=""))



  if (is.null(CVfit) ) {
    if (is.null(sim) )
      sim <- similarityweight(conditionvals,CVdata[conditionvars], threshold=threshold)

    if (is.null (dataplot)) dataplot <- sectionPlotdata
    cols <- weightcolor(CVdata$pointCols, sim)
    # CVdata$pointCols <- NULL
    if (length(sectionvar)==1)
      sectionPlotFN(CVdata,NULL,sectionvar,response, sim,NULL,linecols=linecols,
                    xlim=xlim,ylim=ylim,pointSize=pointSize)
      else
    dataplot(CVdata,c(response,sectionvar),  cols,sim)
  }
  else {
    if (is.null(sim) && showdata)
      sim <- similarityweight(conditionvals,CVdata[conditionvars], threshold=threshold)

    if (!inherits(CVfit, "list"))  CVfit <- list(CVfit)
    if (is.null(names(CVfit)))
      names(CVfit) <- paste0("fit", 1:length(CVfit))
    fitnames <- names(CVfit)



      #if (sp == "fnn" && probs && any(sapply(CVfit, hasprobs,CVdata,levels(CVdata[[response]]))))
        if (sp == "fnn" && probs &&
            any(sapply(1:length(CVfit), function(i) hasprobs(CVfit[[i]], CVdata, predictArgs=predictArgs[[i]])))){
        sectionPlotd3prob(CVdata,CVfit,sectionvar,response, conditionvals,xlim=xlim,ylim=ylim,predictArgs=predictArgs)
        }
      else {
         sectionvals <- lapply(sectionvar, function(p)
            if ( is.factor(CVdata[[p]]))
              levels(CVdata[[p]])
            else  seq(min(CVdata[[p]]),max(CVdata[[p]]),length.out=gridsize)
          )

         names(sectionvals)<- sectionvar
         sectionvals <- expand.grid(sectionvals)
         grid <- conditionvals
         class(grid)<- "list"
         grid[sectionvar] <- sectionvals[sectionvar]
         grid <- as.data.frame(grid)
         grid1 <- grid
         if (is.factor(CVdata[[response]]))
            ylevels <- levels(CVdata[[response]])
          else ylevels <- NULL

          if (length(predictArgs) == length(CVfit))
            for (i  in 1:length(CVfit)){
              fitname <- fitnames[i]
              f <- do.call(CVpredict,  c(list(CVfit[[i]],grid1,ylevels=ylevels), predictArgs[[i]]))

              if (is.character(f) & !is.null(ylevels)) f <- factor(f, ylevels)
              grid[[fitname]] <- f
            }
          else
          for (i  in 1:length(CVfit)){
            fitname <- fitnames[i]
            f <- CVpredict(CVfit[[i]], grid1, ylevels=ylevels)
            if (is.character(f) & !is.null(ylevels)) f <- factor(f, ylevels)
            grid[[fitname]] <- f
          }

          sectionPlotFN <- get(paste(c("sectionPlot",sp),collapse=""))
         
          # if (type=="gg") sectionPlotFN <-paste0("gg",sectionPlotFN)
          if (sp == "nnn" && view3d){
            sectionPlot3D(CVdata,CVfit,fitnames,sectionvar,response, sim,grid,linecols=linecols,
                             theta3d = theta3d, phi3d = phi3d, xlim=xlim,ylim=ylim, zlim=zlim,
                          pointSize=pointSize, density=density,showdata=showdata, predictArgs=predictArgs)
          }
          else {

          sectionPlotFN(CVdata,fitnames,sectionvar,response, sim,grid,linecols=linecols,
                        xlim=xlim,ylim=ylim,zlim=zlim,pointSize=pointSize, density=density,showdata=showdata,
                        returnCoords=returnCoords)
          }
      }
  }
}




sectionPlotd3 <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols, fitcolfn=NULL,pointSize,
                          density=FALSE,showdata,... ){

  par(mar = c(3, 3, 3,.5),
      mgp = c(2, 0.4, 0),
      tck = -.01)

  # if (is.null(fitcolfn)) fitcolfn <- colorfn(CVdata[[response]])
  gx <- grid[[sectionvar[1]]]
  gy <- grid[[sectionvar[2]]]
  xoffset <- gx[2]- gx[1]
  yoffset<- min(gy[gy>gy[1]]) - gy[1]

  fudgex <- .07*xoffset
  fudgey <- .07*yoffset

  if (showdata){
  pcols <- CVdata$pointCols
  o <- sim>0

  pcolso <- pcols[o]
  # pfillso = fitcolfn(CVdata[o,response])

  if (!density)
  pfillso = fitcolfn(CVdata[o,response])
  else pfillso <- NULL

  CVdata1 <- CVdata[o,]
  pointsize <- (sim[o]*.7 + .3)*pointSize
  }
  m <- rbind(seq(along=fitnames), length(fitnames)+1)

  if(isRunning() & length(fitnames)==1) {
    m <- rbind(c(0,1,0), c(0,2,0))
    layout(mat = m,heights = c(.9,.1), widths=c(.17,.66,.17))
  }
  else {m <- rbind(seq(along=fitnames), length(fitnames)+1)
  layout(mat = m,heights = c(.9,.1))
  }
  
  # if(isRunning() & length(fitnames)==1) {
  #   
  #   pmar <- par("mar")
  #   pmar[2]<- 9
  #   pmar[4]<- 9
  #   par(mar=pmar)
  # }
  
  
  for (i in seq(along=fitnames)){
    gf <- grid[[fitnames[i]]]


    plot(c(min(gx)-xoffset,max(gx)+xoffset),  c(min(gy)-yoffset,max(gy)+yoffset),  type="n",xlab=sectionvar[1],
         ylab=if(i==1) sectionvar[2] else "", main=fitnames[i], xaxs="i", yaxs="i")

    col <- fitcolfn(gf)

    rect(gx-xoffset, gy-yoffset,gx+xoffset+fudgex,gy+yoffset+fudgey, col=col,lty=0)
    if (showdata)
    points(CVdata1[[sectionvar[1]]],CVdata1[[sectionvar[2]]],bg=pfillso,col=pcolso, pch=21, cex=pointsize)
  }
}

sectionPlotnnn <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,density=FALSE,zlim=NULL,...){

  if (is.null(zlim) && density){
      ymax <- sapply(fitnames, function(fn) max(grid[[fn]]))
      zlim <- c(0, max(ymax))
    }

  if (is.null(zlim))
  fitcolfn <- colorfn(CVdata[[response]], density=density)
  else fitcolfn <- colorfn(zlim, density=density)

  sectionPlotd3(CVdata,fitnames,sectionvar,response,sim,grid,linecols, fitcolfn=fitcolfn,density=density,...)
  legendn(fitcolfn)
  par(mfrow=c(1,1))
}


sectionPlotnnf <- function(CVdata,fitnames,sectionvar,response, sim,grid,
                           jitter=NULL,linecols,drawaxes=TRUE,ylab=response,pointSize,showdata,returnCoords,...){

  par(mar = c(3, 3, 3,.5),
      mgp = c(2, 0.4, 0),
      tck = -.01,
      mfrow = c(1, length(fitnames)))


  #pcols <- alpha(CVdata[["pointCols"]],sim)
  xvar <- sectionvar[1]
  fac <- sectionvar[2]
  faclevels <- levels(CVdata[[fac]])
  if (length(linecols) < length(faclevels))
    if (length(faclevels) <= 8)
      linecols <- rev(RColorBrewer::brewer.pal(max(3, length(faclevels)), "Dark2"))[1:length(faclevels)]
  else linecols <- colors()[1:length(faclevels)]


  x <- CVdata[[xvar]]
  y <- CVdata[[response]]
  xlim <- range(x)
  ylim <- range(y)
  if (showdata){
  pcols <- weightcolor(CVdata$pointCols, sim)
  o <- attr(pcols, "order")
  pcols <- pcols[o]
  x <- x[o]
  y <- y[o]
  if (!is.null(jitter)){
    x <- jitter(x, amount=jitter[1])
    y <- jitter(y,amount=jitter[2])
    xlim <- c(xlim[1]- jitter[1], xlim[2]+ jitter[1])
    ylim <- c(ylim[1]- jitter[2], ylim[2]+ jitter[2])
  }
  }
  else {
    x <-NULL
    y <- NULL
    pcols <- NULL
  }
  # plot(x, y, col=pcols,xlim=xlim,ylim=ylim,
  #      xlab=sectionvar, ylab=response,pch=20,cex=2,...)

  if(isRunning() & length(fitnames)==1) {
    ppar <- par("pin")
    ppar[1] <- min(ppar[1], 1.4*ppar[2])
    par(pin=ppar)
  }

  for (j in seq(along=fitnames)){
    fn <- fitnames[j]

    plot(x, y, col=pcols, xlim=xlim,ylim=ylim,
         xlab=xvar, ylab=ylab,pch=20,cex=pointSize,
         axes= isTRUE(drawaxes),
         main=fitnames[j])
    if (is.function(drawaxes))
      drawaxes()

    for (i in seq(along=faclevels)){
      rows <- grid[[fac]] == faclevels[i]
      lines(grid[rows,xvar], grid[rows,fn], col=linecols[i], lwd=2.5)
    }

    if (j ==1 & length(faclevels) >0)
      legend("topright", legend = faclevels, col = linecols, lwd=2.5,bty="n", title =fac, cex=.7)

  }

  if (returnCoords && length(fitnames) ==1 && !is.null(x)){
    clickCoords <- data.frame(x=x,y=y,casenum=o)
    return (clickCoords)
  }
  else return(NULL)

}

sectionPlotnfn <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  sectionPlotnnf(CVdata,fitnames,rev(sectionvar),response, sim,grid,linecols= linecols,...)
}

sectionPlotnff <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  levels1 <- levels(CVdata[[sectionvar[1]]])
  levels2 <- levels(CVdata[[sectionvar[2]]])
  if (length(levels2) > length(levels1)){
    sectionvar <- rev(sectionvar)
    levels1 <- levels2
  }
  CVdata[[sectionvar[1]]] <- as.numeric(CVdata[[sectionvar[1]]])
  drawaxes <- function(){
    axis(2)
    axis(1, at=seq(along=levels1), labels=levels1)
  }
  sectionPlotnnf(CVdata,fitnames,sectionvar,response,sim,grid, jitter=c(.03,0),linecols, drawaxes=drawaxes,...)

}

sectionPlotfnf <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  levs <- levels(CVdata[[response]])

  fitp <- any(sapply(grid[,fitnames], function(p) is.double(p) && all(p >= 0) && all(p <=1)))
  grid <- makeFnumeric(grid,fitnames, prob=fitp)
  CVdata <- makeYnumeric(CVdata,response,prob=fitp)

  if (fitp) {
    ticy <- (0:4)/4
    ylab <- paste0("prob(", response, "=", levs[2], ")")
    levs <- ticy
  }
  else {
    ticy <- seq(along=levs)
    ylab <- response
  }

  drawaxes <- function(){
    axis(2, at=ticy, labels=levs)
    axis(1)
  }

  sectionPlotnnf(CVdata,fitnames,sectionvar,response, sim,grid,jitter=c(0,.03),
                 linecols,drawaxes=drawaxes,ylab=ylab,...)
}



sectionPlotffn <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  sectionPlotfnf(CVdata,fitnames,rev(sectionvar),response, sim,grid,linecols,...)
}

sectionPlotfnn <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  levs <- levels(CVdata[[response]])
  fitp <- any(sapply(grid[,fitnames], function(p) is.double(p) && all(p >= 0) && all(p <=1)))

  if (fitp){
    colorY <- colorfnfp()
    CVdata <- makeYnumeric(CVdata,response, fitp)
  } else
  colorY <- colorfnf(CVdata[[response]])
  sectionPlotd3(CVdata,fitnames,sectionvar,response,sim,grid,linecols,fitcolfn=colorY,...)

  if (fitp)
    legendn(colorY)
 else legendf(colorY)
  par(mfrow=c(1,1))
}


sectionPlotfff <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  levs <- levels(CVdata[[response]])
  levels1 <- levels(CVdata[[sectionvar[1]]])
  levels2 <- levels(CVdata[[sectionvar[2]]])
  if (length(levels2) > length(levels1)){
    sectionvar <- rev(sectionvar)
    levels1 <- levels2
  }

  fitp <- any(sapply(grid[,fitnames], function(p) is.double(p) && all(p >= 0) && all(p <=1)))

  grid <- makeFnumeric(grid,fitnames, prob=fitp)
  CVdata <- makeYnumeric(CVdata,response, prob=fitp)

  CVdata[,sectionvar[1]] <- as.numeric(CVdata[,sectionvar[1]])

  if (fitp) {
    ticy <- (0:4)/4
    ylab <- paste0("prob(", response, "=", levs[2], ")")
    levs <- ticy
  }
  else {
    ticy <- seq(along=levs)
    ylab <- response
  }

  drawaxes <- function(){
    axis(2, at=ticy, labels=levs)
    axis(1, at=seq(along=levels1), labels=levels1)
  }

  sectionPlotnnf(CVdata,fitnames,sectionvar,response, sim,grid, jitter=c(.03,.03),linecols, drawaxes=drawaxes,ylab=ylab,...)
}

#-------------------------





sectionPlotd2 <- function(CVdata,fitnames,sectionvar,response, sim,grid,
                          jitter=NULL,linecols,xlim=NULL,ylim=NULL,xlab=sectionvar,ylab=response,pointSize=2,
                          density=FALSE,showdata=TRUE,returnCoords=FALSE,...){

   par(mar = c(3, 3, 3,.5),
      mgp = c(2, 0.4, 0),
      tck = -.01)

  if (length(linecols) < length(fitnames))
    if (length(fitnames) <= 8)
      linecols <- rev(RColorBrewer::brewer.pal(max(3, length(fitnames)), "Dark2"))[1:length(fitnames)]
    else linecols <- rainbow(length(fitnames))

   #pcols <- alpha(CVdata[["pointCols"]],sim)
   x <- CVdata[[sectionvar]]
   y <- CVdata[[response]]
   if (density && is.null(ylim)){
     ymax <- sapply(fitnames, function(fn) max(grid[[fn]]))
     ylim <- c(0, max(ymax))
     y <- y*ymax/10
   }
   if (is.null(xlim))  xlim <- range(x)

   if (is.null(ylim))  ylim <- range(y)


   if (showdata){
   pcols <- weightcolor(CVdata$pointCols, sim)
   o <- attr(pcols, "order")
   pcols <- pcols[o]
   x <- x[o]
   y <- y[o]

   if (!is.null(jitter)){
     x <- jitter(x, amount=jitter[1])
     y <- jitter(y,amount=jitter[2])
     xlim <- c(xlim[1]- jitter[1], xlim[2]+ jitter[1])
     ylim <- c(ylim[1]- jitter[2], ylim[2]+ jitter[2])
   }
   if (returnCoords)
   clickCoords <- data.frame(x=x,y=y,casenum=o)
   else clickCoords <- NULL
   }
   else {
     x <- NULL
     y <- NULL
     pcols <- NULL
     clickCoords <- NULL
   }
   # if(isRunning()) {
   #  pmar <- par("mar")
   #  pmar[2]<- 9
   #  pmar[4]<- 9
   #  par(mar=pmar)
   # }
   
   if(isRunning()) {
     ppar <- par("pin")
     ppar[1] <- min(ppar[1], 1.4*ppar[2])
     par(pin=ppar)
   }
   
   # if(isRunning() & length(fitnames)==1) {
   #   m <- rbind(c(0,1,0))
   #   layout(mat = m,heights = 1, widths=c(.17,.66,.17))
   # }
   # zx <<- par()
    plot(x, y, col=pcols,xlim=xlim,ylim=ylim,
       xlab=xlab, ylab=ylab,pch=20,cex=pointSize,main="",...)
  if (!is.null(grid)){

  for (i in 1:length(fitnames))
    lines(grid[,sectionvar], grid[,fitnames[i]], col=linecols[i], lwd=2.5)
    if (length(fitnames)> 1)
      legend("topright", legend = fitnames, col = linecols, lwd=2.5,bty="n", cex=.7)
  }

    return(clickCoords)

}



sectionPlotnn <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  sectionPlotd2(CVdata,fitnames,sectionvar,response,sim,grid,linecols=linecols,...)
}

sectionPlotnf <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,...){
  levs <- levels(CVdata[[sectionvar]])
  CVdata[[sectionvar]]<- as.numeric(CVdata[[sectionvar]])
  clickc <- sectionPlotd2(CVdata,fitnames,sectionvar,response,sim,grid,jitter=c(0.03,0),linecols=linecols,
                axes=F,...)

  axis(1, at=seq(along=levs), labels=levs)
  axis(2)
  return(clickc)
}

sectionPlotfn <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,xlim,ylim,...){
  levs <- levels(CVdata[[response]])

  CVdata <- makeYnumeric(CVdata,response)
  if (!is.null(grid)){
  fitp <- any(sapply(grid[,fitnames], function(p) is.double(p) && all(p >= 0) && all(p <=1)))

  grid <- makeFnumeric(grid,fitnames, prob=fitp)
  CVdata <- makeYnumeric(CVdata,response, prob=fitp)
  }
  else fitp <- FALSE
  if (fitp) {
    ticy <- (0:4)/4
    ylab <- paste0("prob(", response, "=", levs[2], ")")
    levs <- ticy
  }
  else {
    ticy <- seq(along=levs)
    ylab=response
  }

  ylim1 <- c(ticy[1]-.03, tail(ticy,1)+.03 )
  if (is.null(ylim)) ylim <- ylim1
  else ylim <- c(max(ylim[1], ylim1[1]), min(ylim[2], ylim1[2]))

  clickc <- sectionPlotd2(CVdata,fitnames,sectionvar,response,sim,grid, jitter=c(0,.03),linecols=linecols,
                axes=F, xlim=xlim,ylim=ylim, ylab=ylab,...)
  axis(1)
  axis(2, at=ticy, labels=levs)

  return(clickc)
}

sectionPlotff <- function(CVdata,fitnames,sectionvar,response, sim,grid,linecols,xlim,ylim,...){
    levs <- levels(CVdata[[sectionvar]])
   levsr <- levels(CVdata[[response]])
   if (!is.null(grid)){
   fitp <- any(sapply(grid[,fitnames], function(p) is.double(p) && all(p >= 0) && all(p <=1)))


   CVdata <- makeYnumeric(CVdata,response, fitp)
   CVdata[[sectionvar]] <- as.numeric(CVdata[[sectionvar]])

   grid <- makeFnumeric(grid,fitnames, fitp)
   }
   else fitp <- FALSE

   if (fitp) {
     ticy <- (0:4)/4
     ylab <- paste0("prob(", response, "=", levsr[2], ")")
     levsr <- ticy
   }
   else {
     ticy <- seq(along=levsr)
    ylab <- response
   }

   ylim1 <- c(ticy[1]-.03, tail(ticy,1)+.03 )
   if (is.null(ylim)) ylim <- ylim1
   else ylim <- c(max(ylim[1], ylim1[1]), min(ylim[2], ylim1[2]))

   clickCoords <- sectionPlotd2(CVdata,fitnames,sectionvar,response,sim,grid,jitter=c(0.03,0.03),linecols=linecols,
                 axes=F, xlim=xlim,ylim=ylim, ylab=ylab,...)
   axis(1, at=seq(along=levs), labels=levs)
   axis(2, at=ticy, labels=levsr)
   return(clickCoords)

}


makeYnumeric <- function(CVdata, response,prob=FALSE){
  CVdata[[response]]<- as.numeric(CVdata[[response]])
  if (prob) CVdata[[response]]<-CVdata[[response]]-1
  CVdata
}

makeFnumeric <- function(grid, fitnames, prob=FALSE){
  for (f in fitnames){
    if (is.factor(grid[[f]])){
      grid[[f]] <- as.numeric(grid[[f]])
      if (prob) grid[[f]] <- grid[[f]]-1
    }
  }
  grid
}

sectionPlotdata <- function(CVdata, sectionvars, cols,sim,...){
  if (length(sectionvars) ==1){
    index <- 1:nrow(CVdata)
    CVdata <- rbind(index,CVdata)
  }
  else CVdata <- CVdata[,sectionvars]
  for ( i in 1:ncol(CVdata)){
    if (is.factor(CVdata[[i]]))
      CVdata[[i]]<- as.numeric(CVdata[[i]])
  }
  nums <- sapply(CVdata, is.numeric)
  CVdata <- as.matrix(CVdata[nums])

  if (nrow(CVdata) != 0){
    lwd <- rep(1, nrow(CVdata))
    lwd[sim==1]<- 2

    o <- attr(cols, "order")
    xo <- which(!(1:nrow(CVdata) %in% o))
    o <- c(o,xo)


    parcoord1(CVdata[o,], cols[o],lwd=lwd[o],...)
    axis(2)
  }
}

parcoord1 <-
  function (x, col = 1, lty = 1, horiz=TRUE, autoscale=TRUE,var.label = FALSE, ...)
  {
    if (autoscale){
    rx <- apply(x, 2L, range, na.rm = TRUE)
    rx1 <- rx[2,]- rx[1,]
    if (max(rx1)/min(rx1) > 4)
      x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x,
                                                                    na.rm = TRUE) - min(x, na.rm = TRUE)))
    }
    axisr <- range(x)

    if (horiz){

      matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty,
              xlab = "", ylab = "", axes = FALSE, ...)
      axis(1, at = 1L:ncol(x), labels = colnames(x))
      for (i in 1L:ncol(x)) {
        lines(c(i, i), axisr, col = "grey70")
        if (var.label)
          text(c(i, i), axisr, labels = format(rx[, i], digits = 3),
               xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
      }
    }
    else {
      matplot(t(x), 1L:ncol(x),  type = "l", col = col, lty = lty,
              xlab = "", ylab = "", axes = FALSE, ...)
      axis(2, at = 1L:ncol(x), labels = colnames(x))
      for (i in 1L:ncol(x)) {
        lines(axisr,c(i, i),  col = "grey70")
        if (var.label)
          text(axisr,c(i, i),  labels = format(rx[, i], digits = 3),
               xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
      }
    }
    invisible()
  }


CVenv <- vector(mode="list")
CVenv$densityCols <- blues9
CVenv$responseCols <- RColorBrewer::brewer.pal(11, "PuOr")
CVenv$probCols <- colorRampPalette(RColorBrewer::brewer.pal(4, "Accent")[c(2,4)])(11)

colorfn <- function(vec, cols= NULL, expand=.07, density=FALSE){
  if (is.null(cols))
    if (density) cols <- CVenv$densityCols
    else cols <- CVenv$responseCols

  r <- range(vec)
  if (diff(r) == 0){
    r <- c(r[1]-.5, r[1]+.5)
  }
  else {
    fudge <- diff(r)*expand
    r[1] <- r[1]- fudge
    r[2] <- r[2]+ fudge
    r <- seq(r[1], r[2],length.out=length(cols)+1)
  }
  fn <- function(x){
    index <- as.numeric(cut(x,breaks=r, include.lowest=TRUE))
    cols[index]
  }
  structure(fn,breaks=r)
}



colorfnf <- function(vec, cols= NULL){
  levs <- levels(vec)
  if (is.null(cols)){
    if (length(levs) <= 8){
      #cols <- RColorBrewer::brewer.pal(max(3, length(levs)), "Set3")[1:length(levs)]

    cols <- RColorBrewer::brewer.pal(max(4, length(levs)), "Accent")
    if (length(levs)==2) cols <- cols[c(2,4)]
    else cols <- cols[1:length(levs)]
    }
    # cols <- c("red", "blue", "yellow","green")
    else cols <- rainbow(length(levs))
  }
  fn <- function(x){
    cols[match(x, levs)]
  }
  structure(fn, levels=levs)
  }



colorfnfp <- function(vec=c(0,1), cols= NULL){
  if (is.null(cols)){
    #cols <- RColorBrewer::brewer.pal(10, "RdBu")[c(3,9)]
    #cols <- RColorBrewer::brewer.pal(12, "Set3")[c(1,10)]
    cols <- CVenv$probCols
  }
  r <- seq(vec[1]-.01, vec[2]+.01,length.out=length(cols) +1)
  fn <- function(x){
    if (is.factor(x)) x <- as.numeric(x)-1
    index<- as.numeric(cut(x,breaks=r, include.lowest=TRUE))
    cols[index]
  }
  structure(fn,breaks=r)
}

legendn <- function(colorY){
 
   if (par("pin")[1]> 5)
    inset<- 20
  else inset <- 12
  r <- attr(colorY, "breaks")
  z1<- r[-length(r)]
  z2<- r[-1]
  rectcols <- colorY(r)
  par(mar=c(1.5,inset,.5,inset))
  plot( c(z1[1], z2[length(z2)]),c(0,1),  ann=FALSE, axes=F, type="n")

  rect(z1,0,z2,1,col=rectcols, lty=0)
  par(mgp = c(2, 0.2, 0))
  axis(1,cex.axis=.7, lwd=0, lwd.ticks=.5 )
}

legendf <- function(colorY){
  r <- attr(colorY, "levels")
  if (par("pin")[1]> 5)
    inset<- 20
  else inset <- 12
  
  z1<- seq(along=r)
  z2<- z1+1
    rectcols <- colorY(r)
  par(mar=c(1.5,inset,.5,inset))
  plot( c(z1[1], z2[length(z2)]),c(0,1),  ann=FALSE, axes=F, type="n")

  rect(z1,0,z2,1,col=rectcols, lty=0)
  par(mgp = c(2, 0.2, 0))
  axis(1,cex.axis=.7, lwd=0, at=(z1+z2)/2, labels=r )
}
