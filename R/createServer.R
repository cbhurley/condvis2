

#' Title Creates a shiny server
#'
#' @param CVfit a list of fits
#' @param CVdata the dataset used for the fit
#' @param response name of response variable
#' @param sectionvars names of at most two sectionvars
#' @param conditionvars names of conditionvars
#' @param predsInit starting value for predicts. Defaults to median, or mode for factors
#' @param cPlotPCP if TRUE, conditionplots are drawn as a single PCP (for more than two conditionvars)
#' @param cPlotn  Shows a sample of this number of points in conditionplots.
#' @param orderConditionVars If supplied, a function to order the Condition Vars
#' @param threshold used for similarity weights, defaults to 1.
#' @param thresholdmax maximum value allowed of threshold.
#' @param linecols vector of colors to be used for fits
#' @param showsim if TRUE, shows sim in conditionplots with points
#' @param dataplot "pcp" or "pairs". If CVfit is NULL, used to plot the data
#' @param probs Logical; if \code{TRUE}, shows predicted class probabilities instead of just predicted classes.
#' @param view3d Logical; if \code{TRUE}, includes option for a three-dimensional  regression surface if possible.
#' @param theta3d,phi3d Angles defining the viewing direction. \code{theta3d} gives the azimuthal direction and \code{phi3d} the colatitude. See \code{\link[graphics]{persp}}.
#' @param predictArgs a list with one entry per fit, giving arguments for CVpredict
#' @param xlim passed on to sectionplot
#'@param ylim passed on to sectionplot
#'@param zlim passed on to sectionplot
#'@param density default FALSE. Use TRUE if model is a density function.
#'@param showdata  If FALSE, data on section not shown.
#' @return a function

createCVServer <- function(CVfit,CVdata=NULL, response=NULL,sectionvars,conditionvars,
                           predsInit=NULL,cPlotPCP=FALSE, cPlotn= 1000,orderConditionVars,threshold=1,thresholdmax, linecols=NULL,
                           showsim=FALSE, dataplot="pcp",probs,
                           view3d,theta3d,phi3d,predictArgs, xlim=NULL,ylim=NULL, zlim=NULL, density=FALSE,
                           showdata=TRUE){


  # if (is.null(CVdata)) CVdata <- extractModelData(CVdata)

  preds <- c(sectionvars, conditionvars)
  CVdata0 <- CVdata[,-ncol(CVdata)] # get rid of colour var
  clickCoords <- NULL

  function(input, output,session) {

    options(warn=-1)

    conditionvars <- setdiff(preds, sectionvars)
    plotnames <- NULL
    condtour <- NULL
    plotrows <- NULL
    allconditions <- NULL

    if (is.numeric(cPlotn) && nrow(CVdata) > cPlotn) plotrows <- sample(nrow(CVdata), cPlotn, replace=FALSE)

    if (threshold >= thresholdmax)
      simInit <- rep(1,nrow(CVdata))
    else if (threshold == 0){
      simInit <- rep(0, nrow(CVdata))
      w <- plyr::adply(CVdata[conditionvars], 1,function(x,y) all(x==y),
                       predsInit[conditionvars],.expand=FALSE)[,2]
      simInit[w]<- 1
     
    }
    else simInit <- similarityweight(predsInit,CVdata[conditionvars], threshold)
    cPlots <- NULL
    rv <- reactiveValues(pset=predsInit, sectionvars=sectionvars, condArr=NULL, CVdata = CVdata, sim=simInit)
    tour <- reactiveValues(pos=0, step="stop")

    ranges <- reactiveValues(x = xlim, y = ylim)

    output$display <- renderPlot({
      CVdata <- rv$CVdata

      clickCoords<<- sectionPlot(CVdata,CVfit,response,preds,sectionvar= rv$sectionvars,
                                 conditionvals=rv$pset, pointColor= NULL,sim=rv$sim, linecols=linecols,
                                 dataplot=dataplot,
                                 probs=probs && input$showprobs, theta3d=input$theta3d,phi3d=phi3d,
                                 view3d=view3d && input$view3d,xlim = ranges$x,
                  ylim=ranges$y,zlim=zlim,predictArgs=predictArgs, resetpar=FALSE, density=density,
                  showdata=showdata, returnCoords=TRUE)
    })



    observeEvent(input$display_click, {
      # clickCoords is a data frame constructed by drawing functions.
      # clicks are not always recorded when using the d3plot. Is it to do with layout?
      # also, does not work with more than one plot via mfrow
      click <- input$display_click
       # print("handling click")

      if (is.data.frame(clickCoords)) {
        w <- nearPoints(clickCoords, click, xvar="x", yvar="y", threshold = 5, maxpoints = 1)
        if (nrow(w) ==1){
          rv$pset[1,conditionvars] <- CVdata[w$casenum, conditionvars]
        }
      }
    })


    observeEvent(input$display_dblclick, {

      brush <- input$display_brush

      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- xlim
        ranges$y <- ylim
      }

    })





    updateCS <- function(){
      s1 <- input$sectionvar
      s2 <- input$sectionvar2
      sectionvars <- rv$sectionvars

      newS <- s1
      if (! is.null(s1)  & ! is.null(s2)){
        if (s2 != "None") newS <- c(newS,s2 )
        if (! is.null(setdiff(newS, sectionvars))){
          rv$sectionvars <- newS
        }
      }
      ranges$x <- xlim
      ranges$y <- ylim
    }


    observe({
      sectionvars <- rv$sectionvars
      conditionvars <<- setdiff(preds, sectionvars)
      if (length(conditionvars) ==0) conditionvars <- NULL
      if (is.null(conditionvars))
        condArr <- NULL
      else if (cPlotPCP)
        condArr <- list(conditionvars)
      else if (is.null(orderConditionVars))
        condArr <- pairoff(conditionvars)
      else condArr <- orderConditionVars(CVdata[conditionvars])

      #print(condArr)
      id <- paste0(sample(LETTERS[1:4], 4, replace=T),collapse="")
      if (is.null(condArr)){
        plotnames <<-NULL
      }
      else {
      plotnames <<- paste0("cplots", id, 1:length(condArr))
      createCPlots(condArr)
      }
      rv$condArr <- condArr
    })

    observe({
      if (showdata){
        rv$condArr # in to generate a recalculation of sim
      if (input$threshold >= thresholdmax || is.null(conditionvars))
        sim <- rep(1, nrow(CVdata))
      else if (input$threshold == 0){

        sim <- rep(0, nrow(CVdata))
        w <- plyr::adply(CVdata[conditionvars], 1,function(x,y) all(x==y),
                         rv$pset[conditionvars],.expand=FALSE)[,2]
        sim[w]<- 1

      }
      else sim <- similarityweight(rv$pset,CVdata[conditionvars], input$threshold, input$dist)

      rv$sim <- sim
      }
    })


    observe({
      # print(input$tour)
      rv$condArr
      mkpath <- get(input$tour)

      if (is.function(mkpath)) {
        condtour <<- mkpath(CVdata0,CVfit,input$tourlen, conditionvars=conditionvars,predictArgs=predictArgs)
        condtour <<-pathInterpolate(as.data.frame(condtour),input$ninterp)
      }
      else condtour <<- mkpath
     
      # print("updating slider")
      #print(input$ninterp)
      updateSliderInput(session, "tourstep", value=0,
                        max=nrow(condtour)/(input$ninterp+1), step=round(1/(input$ninterp+1),2))
    })


    observeEvent(input$tourstep,{
      if (input$tourstep > 0 & !is.null(condtour)){

        # print("tourstep")
        # print(input$tourstep)
        index <- round(input$tourstep*(input$ninterp+1))


        rv$pset[1,conditionvars] <- condtour[index,conditionvars]
        
      }
    })


    observeEvent(input$colourvar, {
      rv$CVdata <- pointColor2var(rv$CVdata, input$colourvar)
    })

    observeEvent(input$sectionvar, {
      updateCS()
    })

    observeEvent(input$sectionvar2, {
      updateCS()
    })

    observeEvent(rv$pset, {
      allconditions <<- rbind(allconditions, rv$pset)
    })

    observeEvent(input$quit,{
      rownames(allconditions)<- NULL
      stopApp(allconditions)
    })

    output$conditionInfo <- renderPrint({
      if (!is.null(rv$condArr)){
      ci <- rv$pset[unlist(rv$condArr)]
      print(ci,digits=3,row.names=F)
      }
    })

    output$select2 <- renderUI({
      sv <- isolate(rv$sectionvars)
      sv2 <- sv[2]
      if (is.na(sv2) | sv[2]==sv[1]) sv2 <- "None"
      #print("in select 2")
      selectInput(inputId = "sectionvar2",
                  label = "Second sectionvar",
                  choices = c("None", setdiff(preds, input$sectionvar)),
                  width=220,
                  selected = sv2
      )
    })

    output$cplots <- renderUI({
      if (cPlotPCP) height<- min(800,(length(conditionvars)-1)*150) else
        height<- min(220, round(800/length(rv$condArr)))
      plot_output_list <- lapply(1:length(rv$condArr), function(i) {
        plotname <- plotnames[i]
        plotOutput(plotname, height = height, width = 220, click=paste0(plotname,"Click"))
      })
      do.call(tagList, plot_output_list)
    })


    createCPlots <- function(arr){
      #arr <- rv$condArr

      cPlots <<- vector("list", length(arr))
      for (j in 1:length(arr)) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          i <- j
          plotname <- plotnames[i]
          clickname <- paste0(plotname,"Click")
          var <- arr[[i]]

          if (showsim)
            output[[plotname]] <- renderPlot({
              conditionPlot(rv$CVdata, var, rv$pset,pointColor= NULL,sim= rv$sim, resetpar=FALSE, plotrows=plotrows)
            })
          else
            output[[plotname]] <- renderPlot({
              conditionPlot(rv$CVdata, var, rv$pset,pointColor= NULL,sim= NULL, resetpar=FALSE, plotrows=plotrows)
            })

          observeEvent(input[[clickname]],{
            click <- input[[clickname]]
            res <-conditionClick(CVdata,var,click,plotrows=plotrows)
            # print(res)
            rv$pset[1,names(res)]<- res

          })
        })
      } # end for
    }


  }
}


