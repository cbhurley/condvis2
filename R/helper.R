
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
#' @param data the dataset used for the fit. Should not have NAs for response, sectionvars or conditionvars.
#' @param model A fitted model or list of models. May be NULL.
#' @param response name of response variable. If null, tries to extract from model.
#' @param sectionvars names of sectionvars. If null, extracts from data.
#' @param conditionvars names of condition vars. If null, extracts from data.
#'@param predsInit Optionally provide starting value for some predictors. Defaults to medoid.
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
#' @param dataplot "pcp" or "pairs". Used when there is no response, or more than two sectionvars.
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
#' @examples
#' fit <- lm(mpg ~ wt+hp+am, data=mtcars)
#' if(interactive()){
#' condvis(mtcars,fit, response="mpg",sectionvars="wt", conditionvars=c("am", "hp"), pointColor ="red")
#' }


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
                    orderConditionVars="default", threshold=1, thresholdmax=8*threshold,
                    linecols=NULL,showsim=NULL, theta3d = 45, phi3d = 20,
                    dataplot="pcp", tours=NULL, predictArgs=NULL,xlim=NULL,ylim=NULL,zlim=NULL,density=FALSE,
                    showdata= density==FALSE,displayHeight=950) {

  if (!is.data.frame(data) ) 
    stop("'data' must be a data.frame")
  
  if (thresholdmax==0) thresholdmax <-1
  if (is.null(model)) showdata<- TRUE
  
  if (orderConditionVars=="default")
    orderConditionVars <- if (!cPlotPCP) arrangeC else arrangePCP
  
  
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
    # response <- NULL
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
  
  np <- names(predsInit)
  np1 <- setdiff(names(datar), np)
  
  
  # predsInit1 <- datar[1,]
  # 
  # 
  # predsVal <- lapply(datar, function(p) {
  #   if (is.factor(p)) names( which.max(table(p)))
  #   else median(p)
  # })
  # predsInit1[1,]<- predsVal
  
  if (length(np1)> 1){
    predsInit1 <- topMedoid(data[,np1,drop=FALSE], 1) 
  }
  
  
  
  predsInit2 <- datar[1,,drop=F]
  
  if (length(np1)> 1){
    predsInit2[1,np]<- predsInit[1,np]
    predsInit2[1,np1]<- predsInit1[1,np1]
  }
  else predsInit2[1,np]<- predsInit[1,np]
  


  # data <- pointColor2var(data, pointColor[1])

  if (is.null(showsim)) showsim <- nrow(data)<= 150 && showdata

  if (thresholdmax==0) thresholdmax <-1
  # if (length(sectionvars)==1) sectionvars <- c(sectionvars, "None")
  ui <- createCVUI(model,data,response,sectionvars,preds,pointColor,threshold, thresholdmax,tours, probs, view3d)

  server <- createCVServer(model,data, response,sectionvars,conditionvars,predsInit1,pointColor,
                           cPlotPCP = cPlotPCP, cPlotn = cPlotn,
                           orderConditionVars=orderConditionVars, 
                           threshold=threshold,thresholdmax=thresholdmax, linecols=linecols, showsim=showsim,
                           dataplot=dataplot,theta3d, phi3d, probs=probs, view3d=view3d,
                           predictArgs=predictArgs,xlim=xlim,ylim=ylim, zlim=zlim,density=density,
                           showdata=showdata)
  s <-shiny::shinyApp(ui, server,options=list(width="100%", height=displayHeight, width=700))
  if(interactive()) 
    runApp(s) 
   else s
}





