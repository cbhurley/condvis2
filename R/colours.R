CVenv <- vector(mode="list")
CVenv$densityCols <- blues9
CVenv$responseCols <- RColorBrewer::brewer.pal(11, "PuOr")
CVenv$probCols <- colorRampPalette(RColorBrewer::brewer.pal(4, "Accent")[c(2,4)])(11)
# CVenv$probCols <- colorRampPalette(RColorBrewer::brewer.pal(11, "PRGn")[8:4])(100)


CVenv$probCols <- scales::div_gradient_pal(low=RColorBrewer::brewer.pal(3, "Accent")[1],
                                           mid="white",
                                           high=RColorBrewer::brewer.pal(3, "Accent")[2])(seq(0,1,length.out=20))


colorfn <- function(vec, cols= NULL, expand=.07, density=FALSE){
  if (is.null(cols))
    if (density) cols <- CVenv$densityCols
    else cols <- CVenv$responseCols
    
    r <- range(vec, na.rm = TRUE)
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
      if (length(levs)==2) cols <- cols[c(1,2)]
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





pointColor2var <- function(data, pointColor, legend=FALSE){
  
  if (pointColor %in% names(data) & is.numeric(data[[pointColor]])){
    newname <- paste0(pointColor,"F3")
    data[[newname]] <- cut(data[[pointColor]],3)
    pointColor <- newname
  }
  
  pcols <- NULL
  if (pointColor %in% names(data)){
    levs <- levels(data[[pointColor]])
    pcols <-rev(scales::hue_pal()(max(4,length(levs))))[1:length(levs)]
    names(pcols)<- levs
    pointCols <- pcols[as.numeric(data[[pointColor]])]
  } else pointCols <- pointColor
  
  
  data$pointCols <- pointCols
  if (legend  ) list(data=data, cols=pcols, cvar=pointColor)
  else data
}



#' Fade colours according to a weight vector
#'
#' The colours whose weights are less than 1 are diluted. Colours whose weight is zero are returned as white, 
#' other weights are grouped in \code{nlevels} groups and colours diluted proportionally.
#' 
#' @param col A vector of colour
#' @param weights A vector of weights, values between 0 and 1
#' @param nlevels  The number of groups
#'
#' @return A vector of colours
#' @export
#'

weightcolor <-
  function(col, weights, nlevels=5)
  {
    
    n <- length(weights)
    if (length(col) ==1)
      col <- rep(col, length.out = n)
    
    ## Discretise `weights`. We just want nlevels different shades
    
    # if (nlevels==3)
    #   wmax <- c(0, 0.4, 0.7, 1) # Mark's settings
    # else 
    wmax <- (0:nlevels)/nlevels
    
    weights <- wmax[findInterval(weights, c(0, .Machine$double.eps,
                                            wmax[-1]), rightmost.closed = TRUE)]
    
    
    ## We won't perform calculations on elements with `weight` == 0.
    
    weightsgr0 <- which(weights > 0)
    data.order <- weightsgr0[order(weights[weightsgr0])]
    
    ## Linearly fade the colours in `col` to white in RGB space according to their
    ## `weights`.
    
    # newcol <- (col2rgb(col[data.order]) * matrix(rep(weights[data.order], 3),
    #                                              nrow = 3, byrow = TRUE) / 255) + matrix(rep(1 - weights[data.order], 3),
    #                                                                                      nrow = 3, byrow = TRUE)
    #
    newcol1 <- t(col2rgb(col[data.order])) * weights[data.order]/255+ 1-weights[data.order]
    
    data.colour <- rep(NA, n)
    # data.colour[data.order] <- rgb(t(newcol))
    data.colour[data.order] <- rgb(newcol1)
    
    ## Return the weighted colours with the order as attribute.
    
    structure(data.colour, order = data.order)
  }

