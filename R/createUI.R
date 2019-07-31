


#' Constructs UI for Condvis
#' @param CVfit a list of fits
#' @param data a dataset
#' @param response name of response variable
#' @param preds names of predictors
#' @param sectionvars names of sectionvars
#' @param pointColor a color, or the name of variable to be used for coloring
#' @param threshold used for similarity weights, defaults to 1.
#' @param thresholdmax maximum value allowed of threshold.
#' @param tours A list of pre-calculated tours
#' @param probs Logical; if \code{TRUE}, shows predicted class probabilities instead of just predicted classes.
#' @param view3d Logical; if \code{TRUE}, includes option for a three-dimensional regression surface if possible.

#'@return a dataframe of conditions
createCVUI <- function(CVfit,data,response,sectionvars,preds=NULL, pointColor,threshold=1,thresholdmax, tours,probs,
                       view3d){
  colorvars <- sapply(data, is.factor)
  colorvars <- union(pointColor, names(data)[colorvars])
  responsePlot <- !is.null(response) & length(sectionvars) <=2
  if (nrow(data) <= 1000)
  tours1 <- list("Random"= "randomPath",
       "Kmeans"= "kmeansPath",
       "Kmed"= "pamPath")
  else tours1 <- list("Random"= "randomPath",
                      "Kmeans"= "kmeansPath",
                      "Kmed"= "fastkmedPath")
  if (!is.null(CVfit)){
    # tours1 <- c(tours1, list("Lack of fit" = "lofPath"))
    # rework lofPath to calc y -yhat. needs to check there is response
    if (length(CVfit)> 1)
      tours1 <- c(tours1, list("Diff fits" = "diffitsPath"))
  }

  if ( !is.null(tours) & is.null(names(tours)))
    names(tours) <- paste0("Tour", seq(along=tours))
  tours <- c(tours1, tours)

  fluidPage(
     # h3("Condvis"),
     tags$style(type = "text/css",
               "label { font-size: 12px; }"
    ),
    tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
                              font-size: 12px;}"))),
    tags$head(tags$style("#quit{font-size: 12px;}")),

    sidebarLayout(sidebarPanel(
      # selectInput(inputId = "sectionvar",
      #                                      label = "Choose a sectionvar",
      #                                      choices = preds,
      #                                      width=220,
      #                                      selected = sectionvars[1]
      #                                      ),
                             #  uiOutput("select2"),
                               uiOutput("cplots"),
                               tags$br(),
                               # tags$br(),
                               verbatimTextOutput("conditionInfo"),
                               width=4,
                               tags$head(tags$style("#conditionInfo{font-size: 9px;}")),
                                actionButton("quit", "Return conditions")
                               ),


    mainPanel(fluidRow(if (responsePlot) column(3, offset=1,
                              selectInput(inputId = "sectionvar",
                                          label = "Choose a sectionvar",
                                          choices = preds,
                                          width=220,
                                          selected = sectionvars[1]
                              )),
                       if (responsePlot) column(3, offset=1,uiOutput("select2")),
                       column(3, offset=1,
                              selectInput(inputId = "colourvar",
                                          label = "Choose a colourvar",
                                          choices = colorvars,
                                          width=220,
                                          selected = pointColor[1]
                              ))

                     ),

              if (probs) fluidRow(
                  column(3, offset=1, checkboxInput("showprobs", "Show probs", FALSE))),
              if (view3d)
                conditionalPanel(
                  condition = "input.sectionvar2 != 'None' ",
                fluidRow(
                column(3, offset=1, checkboxInput("view3d", "Show 3d surface", FALSE)),
                column(4, offset=3, 
                       conditionalPanel(
                  condition = "input.view3d==true",
                  fluidRow(
                  column(2, offset=0, "Rotate"),
                  column(6, sliderInput(inputId = "theta3d",ticks=FALSE, step=10,
                              label = NULL,min=0, max=360,value=40,animate=animationOptions(loop=TRUE, interval=200))
                )))))),
             plotOutput("display",click = "display_click",
                        dblclick = "display_dblclick", height = "320px",
                        brush = brushOpts(
                          id = "display_brush", 
                          resetOnNew = TRUE)),
                        # tags$br(),
             
              fluidRow( column(3, offset=1,
                                sliderInput("threshold", "Similarity Threshold", 0,
                                            thresholdmax, threshold, step=min(.2, thresholdmax/20))),
                       column(6, offset=1,
                              radioButtons(inputId = "dist", "Distance",
                                         choices = list("maxnorm" ,"euclidean", "gower"), inline=TRUE))),
             # tags$br(),
            
             fluidRow(column(9, offset=1, wellPanel(
              fluidRow(
               column(4, offset=0, selectInput(inputId = "tour",
                                                      label = "Choose tour",
                                                      choices= tours,
                                                      width=150)
             ),
             # tags$br(), column(2, offset=0, actionButton(inputId = "start",label = "Move")),
             column(6, offset=1, sliderInput(inputId = "tourstep", step=0, round=-1,ticks=FALSE,
                                              label = "Tour Step",min=0, max=10,value=0,
                                             animate=animationOptions(loop=TRUE)))),
             fluidRow(column(4, offset=0, sliderInput(inputId = "tourlen",ticks=FALSE, step=1,
                                             label = "Tour Length",min=5, max=30,value=10
                                             )),

             column(4, offset=1, sliderInput(inputId = "ninterp",ticks=FALSE,
                                             label = "Interp steps",min=0, max=6,value=0,step=1
             )))))
             )
             

             ), position="right")
  )
}



