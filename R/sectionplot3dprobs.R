

sectionPlotpnn <- function(CVdata,CVfit,sectionvar,response, conditionvals,predictArgs=NULL,...) {
  
  par(mar = c(3, 3, 3,.5),
      mgp = c(2, 0.4, 0),
      tck = -.01)
  
  fitcolfn <- colorfnf(CVdata[[response]])

  fitnames <- names(CVfit)
  
  nlevels <- 15
  
  sectionvalsn <- lapply(sectionvar, function(p){
    xvar <- CVdata[[p]]
    if (is.factor(xvar)) 1:length(levels(xvar)) - .5
    else seq(min(xvar),max(xvar),length.out=nlevels)
  }
  )
  
  sectionvals <- lapply(sectionvar, function(p){
    xvar <- CVdata[[p]]
    if (is.factor(xvar))levels(xvar)
    else seq(min(xvar),max(xvar),length.out=nlevels)
  }
  )
  nlevels <- sapply(sectionvals, length)
    
  names(sectionvals)<- sectionvar
  
  sectionvals <- expand.grid(sectionvals)
  sectionvalsn <- expand.grid(sectionvalsn)
  grid <- conditionvals
  class(grid)<- "list"
  grid[sectionvar] <- sectionvals[sectionvar]
  grid <- as.data.frame(grid)

  ylevels <- levels(CVdata[[response]])
  
  w <- 1
  if (length(predictArgs) == length(CVfit))
    p1 <- do.call(CVpredict,  c(list(CVfit[[w]],grid,ylevels=ylevels,ptype="probmatrix"), predictArgs[[w]]))
  else
    p1 <- CVpredict(CVfit[[w]], grid, ylevels=ylevels,ptype="probmatrix")
  
  # gx <- grid[[sectionvar[1]]]
  # gy <- grid[[sectionvar[2]]]
  
  gx <- sectionvalsn[,1]
  gy <- sectionvalsn[,2]
  if (is.factor(CVdata[[sectionvar[1]]])) xoffset <- .5
  else xoffset <- gx[2]- gx[1]
  
  if (is.factor(CVdata[[sectionvar[2]]])) yoffset <- .5
  else yoffset<- min(gy[gy>gy[1]]) - gy[1]
  
  
  cols <- fitcolfn(ylevels)
  
  
  if(isRunning()) {
    m <- rbind(c(0,1,0), c(0,2,0))
    layout(mat = m,heights = c(.9,.1), widths=c(.17,.66,.17))
  }
  else {m <- matrix(1:2)
  layout(mat = m,heights = c(.9,.1))
  }
  
  
  
  plot(c(min(gx)-xoffset,max(gx)+xoffset),  c(min(gy)-yoffset,max(gy)+yoffset),
       type="n",xlab=sectionvar[1], ylab= sectionvar[2], 
       main=if (length(fitnames) > 1) fitnames[w] else "", axes=F)
  totalwidth <- abs(diff(par()$usr[1:2]))
  totalheight <- abs(diff(par()$usr[3:4]))
  
  if  (is.factor(CVdata[[sectionvar[1]]]))
    barw <- 0.4 * totalwidth / nlevels[1]
  else barw <- 0.6 * totalwidth / nlevels[1]
  
  if  (is.factor(CVdata[[sectionvar[2]]]))
    barh <- 0.4 * totalheight / nlevels[2]
  else barh <- 0.6 * totalheight / nlevels[2]
  
  # o1 <- apply(cbind(grid[,sectionvar], p1), 1,
  #             function (x) myglyph2a(x[1], x[2], barw,
  #                                    barh,
  #                                    x[3:(2 + ncol(p1))]))
  
  o1 <- apply(cbind(sectionvalsn, p1), 1,
              function (x) myglyph2a(x[1], x[2], barw,
                                     barh,
                                     x[3:(2 + ncol(p1))]))
  
  o2 <- matrix(t(o1), ncol = 5, byrow = FALSE)
  rect(xleft = o2[, 1], xright = o2[, 2], ybottom = o2[, 3],
       ytop = o2[, 4], col = cols[o2[, 5]], border=NA)
  
  if  (is.factor(CVdata[[sectionvar[1]]]))
    axis(1, at=1:nlevels[1] -.5, labels=levels(CVdata[[sectionvar[1]]]),lwd=0)
  else axis(1)
  if  (is.factor(CVdata[[sectionvar[2]]]))
    axis(2, at=1:nlevels[2] -.25, labels=levels(CVdata[[sectionvar[2]]]),lwd=0,las=2)
  else axis(2)
  
  legendf(fitcolfn)
  par(mfrow=c(1,1))
}


sectionPlotpn <- function(CVdata,CVfit,sectionvar,response, conditionvals,predictArgs=NULL,
                          returnInfo=FALSE,...) {
  
  par(mar = c(3, 3, 3,.5),
      mgp = c(2, 0.4, 0),
      tck = -.01)
  
  fitnames <- names(CVfit)
  fitcolfn <- colorfnf(CVdata[[response]])
  nlevels <- 15
  
  xvar <- CVdata[[sectionvar]]
  if (is.factor(xvar)){
    nlevels <- length(levels(xvar))
    sectionvalsn <-1:nlevels - .5
  } else sectionvalsn <-seq(min(xvar),max(xvar),length.out=nlevels)
  
  
  if (is.factor(xvar)){
    sectionvals <-levels(xvar)
  } else sectionvals <-sectionvalsn
  
  
  grid <- conditionvals
  class(grid)<- "list"
  grid[[sectionvar]] <- sectionvals
  grid <- as.data.frame(grid)
  ylevels <- levels(CVdata[[response]])
  
  w <- 1
  if (length(predictArgs) == length(CVfit))
    p1 <- do.call(CVpredict,  c(list(CVfit[[w]],grid,ylevels=ylevels,ptype="probmatrix"), predictArgs[[w]]))
  else
    p1 <- CVpredict(CVfit[[w]], grid, ylevels=ylevels,ptype="probmatrix")
  
  gx <- sectionvalsn
  
  if (is.factor(xvar)) xoffset <- .5
  else xoffset <- gx[2]- gx[1]
  
  cols <- fitcolfn(ylevels)
  
  
  if(isRunning()) {
    m <- rbind(c(0,1,0), c(0,2,0))
    layout(mat = m,heights = c(.9,.1), widths=c(.17,.66,.17))
  }
  else {m <- matrix(1:2)
  layout(mat = m,heights = c(.9,.1))
 }
  
  plot(c(min(gx)-xoffset,max(gx)+xoffset),  c(0,1),
       type="n",xlab=sectionvar[1], ylab= paste("prob", response),
       main=if (length(fitnames) > 1) fitnames[w] else "", axes=FALSE)
  totalwidth <- abs(diff(par()$usr[1:2]))
  totalheight <- abs(diff(par()$usr[3:4]))
  if (is.factor(xvar))
  barw <- 0.4 * totalwidth / nlevels
  else barw <- 0.6 * totalwidth / nlevels
  o1 <- apply(cbind(sectionvalsn, p1), 1,
              function (x) myglyph2a(x[1], 0,barw,
                                     1,
                                     x[2:(1 + ncol(p1))]))
  
  o2 <- matrix(t(o1), ncol = 5, byrow = FALSE)
  rect(xleft = o2[, 1], xright = o2[, 2], ybottom = o2[, 3],
       ytop = o2[, 4], col = cols[o2[, 5]], border=NA)
  if (is.factor(xvar))
    axis(1, at=1:nlevels -.5, labels=levels(xvar),lwd=0)
  else axis(1)
  

  legendf(fitcolfn)
  par(mfrow=c(1,1))
  
  
}


sectionPlotpf <- function(CVdata,CVfit,sectionvar,response, conditionvals,predictArgs=NULL,...) {
  sectionPlotpn(CVdata,CVfit,sectionvar,response, conditionvals,xlim=xlim,ylim=ylim,
                predictArgs=predictArgs,...)
  
}

sectionPlotpfn <- function(CVdata,CVfit,sectionvar,response, conditionvals,predictArgs=NULL,...) {
  sectionPlotpnn(CVdata,CVfit,rev(sectionvar),response, conditionvals,xlim=xlim,ylim=ylim,
                predictArgs=predictArgs,...)
  
}

sectionPlotpff <- function(CVdata,CVfit,sectionvar,response, conditionvals,predictArgs=NULL,...) {
  sectionPlotpnn(CVdata,CVfit,sectionvar,response, conditionvals,xlim=xlim,ylim=ylim,
                 predictArgs=predictArgs,...)
  
}

sectionPlotpnf <- function(CVdata,CVfit,sectionvar,response, conditionvals,predictArgs=NULL,...) {
  sectionPlotpnn(CVdata,CVfit,sectionvar,response, conditionvals,xlim=xlim,ylim=ylim,
                 predictArgs=predictArgs,...)
  
}


# for horizontal barplots
myglyph2 <-
  function (x, y, xw, yw, heights, col)
  {
    left <- x - 0.5 * xw
    right <- x + 0.5 * xw
    top <- y + 0.5 * yw
    bottom <- y - 0.5 * yw
    barwidth <- xw / length(heights)
    barleft <- seq(left, right - barwidth, barwidth)
    if (any(heights < 0))
      stop("cannot handle negative 'heights'")
    if (any(heights > 1))
      heights <- heights / max(heights)
    cbind(barleft, barleft + barwidth, bottom, bottom + heights*yw, 1:length(heights))
  }


# for stacked barplots
myglyph2a <-
  function (x, y, xw, yw, heights)
  {
    o <- order(heights, decreasing=TRUE)
    cols <- 1:length(heights)
    cols <- cols[o]
    heights <- heights[o]*yw
    left <- x - 0.5 * xw
    right <- x + 0.5 * xw
    # top <- y + 0.5 * yw
    basey <- y
    tops <- cumsum(heights)
    bottoms <- c(0,tops[-length(tops)])
    if (any(heights < 0))
      stop("cannot handle negative 'heights'")
    # if (any(heights > 1))
    #   heights <- heights / max(heights)
    cbind(left,right, basey+bottoms, basey+tops, cols)
  }



sectionPlot3D <- function(CVdata,CVfit,fitnames,sectionvar,response, sim,grid,linecols,
                          theta3d, phi3d,density=FALSE,zlim=NULL,predictArgs=NULL,showdata,... ){
  
  
  
  
  par(mar = c(3, 3, 3,.5),
      mgp = c(2, 0.4, 0),
      tck = -.01)
  
  if (is.null(zlim))
    if (density){
      ymax <- sapply(fitnames, function(fn) max(grid[[fn]]))
      zlim <- c(0, max(ymax))
    }
  else zlim <- range(CVdata[[response]], na.rm = TRUE)
  
  if (is.null(zlim))
    fitcolfn <- colorfn(CVdata[[response]], density=density)
  else fitcolfn <- colorfn(zlim, density=density)
  
  v1 <- sectionvar[1]
  v2 <- sectionvar[2]
  if (showdata){
    pcols <- weightcolor(CVdata$pointCols, sim)
    o <- attr(pcols, "order")
    CVdata1 <- CVdata[o,]
    pcols1 <- pcols[o]
  }
  else o <- NULL
  #par(mfrow=c(1, length(fitnames)))
  
  m <- rbind(seq(along=fitnames), length(fitnames)+1)
  
  layout(mat = m,heights = c(.9,.1))
  preds <- names(grid)
  fitpos <- match(fitnames[1], preds)
  preds <- preds[1:(fitpos - 1)]
  
  for (w in seq(along=fitnames)){
    # yhat <- CVpredict(CVfit[[w]],CVdata[,preds])
    z <- matrix(grid[[fitnames[w]]], ncol = 20L, byrow = FALSE)
    zfacet <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1]
               + z[-nrow(z), -ncol(z)]) / 4
    colorfacet <- fitcolfn(zfacet)
    
    
    persp.object <-
      suppressWarnings(persp(x = unique(grid[, v1]), y = unique(grid[, v2]),
                             border = rgb(0.3, 0.3, 0.3), lwd= 0.1, z = z,
                             col = colorfacet, zlim = zlim,
                             xlab = v1, ylab=v2, zlab=response,
                             d = 10, ticktype = "detailed", 
                             main = if (length(fitnames)>1) fitnames[w] else "",
                             theta = theta3d, phi = phi3d))
    
    if (length(o) > 0 && !density && showdata){
      
      # yhat <- yhat[o]
      paw <- predictArgs[[w]]
      paw$pinterval <- NULL
      
      yhat <- do.call(CVpredict,  c(list(CVfit[[w]],CVdata[o,preds]), pinterval=NULL,paw))
      
      points(trans3d(CVdata1[,v1], CVdata1[,v2], CVdata1[,response],
                     pmat = persp.object), col = pcols1, pch = 20)
      
      linestarts <- trans3d(CVdata1[,v1], CVdata1[,v2], CVdata1[,response], pmat = persp.object)
      lineends <- trans3d(CVdata1[,v1], CVdata1[,v2], yhat, pmat = persp.object)
      segments(x0 = linestarts$x, y0 = linestarts$y, x1 = lineends$x,
               y1 = lineends$y, col = pcols1)
    }
  }
    legendn(fitcolfn)
  par(mfrow=c(1,1))
}
