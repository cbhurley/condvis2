
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#devtools::document() press Ctrl/Cmd + Shift + D in RStudio) to convert roxygen comments to .Rd files.
#devtools::build_vignettes()
# use devtools::build() to create a package bundle with the vignettes included.
# devtools::check() same as R CMD check








#' Creates Condvis Shiny app
#'
#' @param data the dataset used for the fit
#' @param model A fitted model or list of models. May be NULL.
#' @param response name of response variable. If null, tries to extract from model.
#' @param sectionvars names of sectionvars. If null, extracts from data.
#' @param conditionvars names of condition vars. If null, extracts from data.
#'@param predsInit Optionally provide starting value for some predictors. Defaults to median, or mode for factors
#'@param pointColor a color, or the name of variable to be used for coloring. If the named variable is numeric, it is first converted to a factor with 3 levels.
#'@param  cPlotPCP if TRUE, conditionplots are drawn as a single PCP (for more than two conditionvars)
#'@param cPlotn Defaults to 1000. Shows a sample of this number of points in conditionplots. Non-numeric values are ignored.
#'@param orderConditionVars If supplied, a function to order the Condition Vars
#'@param threshold used for similarity weights, defaults to 1.
#'@param thresholdmax maximum value allowed of threshold.
#'@param linecols vector of colors to be used for fits
#'@param showsim if TRUE, shows sim in conditionplots with points/lines. Defaults to TRUE with 150 or fewer cases.

#' @param theta3d,phi3d Angles defining the viewing direction for 3d surface. \code{theta3d}
#'   gives the azimuthal direction and \code{phi3d} the colatitude. See
#'   \code{\link[graphics]{persp}}.
#' @param dataplot if model is NULL, the data are plotted using this function. Defaults to a parallel coordinate plot
#' @param tours A list of pre-calculated tours
#' @param predictArgs a list with one entry per fit, giving arguments for CVpredict
#' @param xlim passed on to sectionplot
#'@param ylim passed on to sectionplot
#'@param zlim passed on to sectionplot
#'@param density default FALSE. Use TRUE if model is a density function.
#'@param showdata defaults to \code{density==TRUE}. If FALSE, data on section not shown.
#' @param displayHeight supply a value for the display height

#' @return NULL
#' @export
#'
#' @examples
#' mtcars$am <- as.factor(mtcars$am)
#'fit <- lm(mpg ~ wt+hp+am, data=mtcars)
#'vars <- all.vars(formula(fit))
#'\dontrun{condvis(fit,mtcars, response=vars[1],vars[2], vars[-(1:2)], "red")}

#' @import ggplot2
#' @import shiny
#' @import grDevices
#' @import methods
#' @import stats
#' @import RColorBrewer
#' @import graphics
#' @import utils

condvis <- function(data,model=NULL, response=NULL,sectionvars=NULL,conditionvars=NULL,
                    predsInit=NULL, pointColor=c("steelblue", "grey0"), cPlotPCP=FALSE,
                    cPlotn = 1000,
                    orderConditionVars=arrangeC, threshold=1, thresholdmax=8*threshold,
                    linecols=NULL,showsim=NULL, theta3d = 45, phi3d = 20,
                    dataplot=NULL, tours=NULL, predictArgs=NULL,xlim=NULL,ylim=NULL,zlim=NULL,density=FALSE,
                    showdata= density==FALSE,displayHeight=900) {

  if (thresholdmax==0) thresholdmax <-1
  if (is.null(model)) showdata<- TRUE
  
  if (density ) {
    response <- "densityY"
    # if (is.null(ylim)){
    # maxy <- sapply(1:length(model), function(w)
    #   do.call(CVpredict,  c(list(model[[w]],data), predictArgs[[w]])))
    #
    # maxy <- max(maxy)
    # # maxy <- max(sapply(model, function(m) max(CVpredict(m,data))))
    # ylim<- c(0,1.05*maxy)
    data$densityY <- runif(nrow(data))
    # if (is.null(zlim)) zlim <- ylim
  }
  if (!is.null(model)){
    if (!inherits(model, "list")) model <- list(model)

    if (is.null(names(model)))
      names(model) <- paste0("fit", 1:length(model))

    if (length(linecols) != length(model))
      if (length(model) <= 8)
        linecols <- rev(RColorBrewer::brewer.pal(max(3, length(model)), "Dark2"))[1:length(model)]
      else linecols <- colors()[1:length(model)]

      if (is.null(response) && !density){
        frm <- try(formula(model[[1]], silent=TRUE))
        if (class(frm) != "try-error")
          response <- all.vars(frm)[1]
        else stop("could not extract response from 'model'.")
      }
      if (!is.null(sectionvars) & !is.null(conditionvars))
        preds <- c(sectionvars, conditionvars)
      else if (is.null (conditionvars) & !is.null(sectionvars)){
        preds <- names(data)
        preds<- preds[preds!=response]
        conditionvars <- setdiff(preds, sectionvars)
      } else if (is.null (sectionvars) & !is.null(conditionvars)){
        preds <- conditionvars
        sectionvars <- preds[1]
        conditionvars <- preds[-1]
      }else {
        preds <- names(data)
        preds<- preds[preds!=response]
        sectionvars <- preds[1]
        conditionvars <- preds[-1]
      }

      


      # probs <- is.factor(data[[response]])  &&
      #   any(sapply(1:length(model),
      #              function(i) hasprobs(model[[i]], data, predictArgs=predictArgs[[i]])))
      # 
      probs <- is.factor(data[[response]])
      view3d <-  is.numeric(data[[response]]) && (sum(sapply(data[,c(sectionvars,conditionvars)], is.numeric)) >=2)



  } else {
    probs <- FALSE
    view3d <- FALSE
    response <- NULL
    if (!is.null(sectionvars) & !is.null(conditionvars))
      preds <- c(sectionvars, conditionvars)
    else if (is.null (conditionvars)){
      preds <- names(data)
      conditionvars <- preds
    } else {
      preds <- names(data)
      sectionvars <- preds
    }
  }
  r <- NULL
  if (!is.null(response)) r <- match(response, names(data))
  if (is.numeric(r)) datar <- data[,-r]
  else datar <- data

  predsInit1 <- datar[1,]
  predsVal <- lapply(datar, function(p) {
    if (is.factor(p)) names( which.max(table(p)))
    else median(p)
  })
  predsInit1[1,]<- predsVal
  if (! is.null(predsInit)){
    np <- intersect(names(predsInit), names(predsInit1))
    predsInit1[1,np] <- predsInit[1,np]
  }

  data <- pointColor2var(data, pointColor[1])

  if (is.null(showsim)) showsim <- nrow(data)<= 150 && showdata

  if (thresholdmax==0) thresholdmax <-1
  # if (length(sectionvars)==1) sectionvars <- c(sectionvars, "None")
  ui <- createCVUI(model,data,sectionvars,preds,pointColor,threshold, thresholdmax,tours, probs, view3d)

  server <- createCVServer(model,data, response,sectionvars,conditionvars,predsInit1,
                           cPlotPCP = cPlotPCP, cPlotn = cPlotn,
                           orderConditionVars=orderConditionVars,
                           threshold=threshold,thresholdmax=thresholdmax, linecols=linecols, showsim=showsim,
                           dataplot=dataplot,theta3d, phi3d, probs=probs, view3d=view3d,
                           predictArgs=predictArgs,xlim=xlim,ylim=ylim, zlim=zlim,density=density,
                           showdata=showdata)
  shiny::shinyApp(ui, server,options=list(width="100%", height=displayHeight))
}




pointColor2var <- function(data, pointColor){

  if (pointColor %in% names(data) & is.numeric(data[[pointColor]])){
    newname <- paste0(pointColor,"F3")
    data[[newname]] <- cut(data[[pointColor]],3)
    pointColor <- newname
  }


  if (pointColor %in% names(data)){
    pointCols <-rev(scales::hue_pal()(max(4,length(levels(data[[pointColor]])))))
    pointCols <- pointCols[as.numeric(data[[pointColor]])]
  } else pointCols <- pointColor

  data$pointCols <- pointCols
  data
}


## Function to weight colours according to a weight vector. Not exported.

weightcolor <-
  function(col, weights, nlevels=5)
  {

    n <- length(weights)
    if (length(col) ==1)
     col <- rep(col, length.out = n)

    ## Discretise `weights`. We just want nlevels different shades

    if (nlevels==3)
      wmax <- c(0, 0.4, 0.7, 1) # Mark's settings
    else wmax <- (0:nlevels)/nlevels

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


