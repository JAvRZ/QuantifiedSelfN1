shiny.installed <- require(shiny)
if( !shiny.installed ){install.packages("shiny"); require(shiny)}
nlme.installed <- require(nlme)
if( !nlme.installed ){install.packages("nlme"); require(nlme)}
foreign.installed <- require(foreign)
if( !foreign.installed ){install.packages("foreign"); require(foreign)}


server <- shinyServer(function(input, output) {

  
  myupload <- reactive({
    input.file <- input$userfile
    
    if (is.null(input.file)){
      mydata <- data.frame()
      return(mydata)
    } else {
      if(length(grep( ".sav$", input.file$name)) > 0){
        mydata <- as.data.frame(read.spss(input.file$datapath))
      }
      
      if(length(grep( ".csv$", input.file$name)) > 0){
        mydata <- as.data.frame(read.csv(input.file$datapath, sep = input$mycsvsep))
      }
      
      if(length(grep( ".txt$|.dat$", input.file$name)) > 0){
        mydata <- as.data.frame(read.table(input.file$datapath, header = TRUE))
        
      }
      return(mydata)
    }
  })
  
  
  output$mycsvInput <- renderUI({
    input.file <- input$userfile
    
    if (!is.null(input.file)){
      if(length(grep( ".csv$", input.file$name)) > 0){
        selectInput(inputId = "mycsvsep", label = 
                      "Which symbol used to separate the columns in a CSV file?",
                    choices = c(",",";"))} else {
                      return(invisible)
                    }
    }
  })
  
  output$phasecolumn <- renderUI({
    input.file <- input$userfile
    if (!is.null(input.file)){
      mycolnames <- colnames(myupload())
      selectInput(inputId = "myphasecolumn", label = 
                    "Which column contains the phases?",
                  choices = mycolnames)
    }
  })
  
  
  mydatatransform.selectphase <- reactive({
    
    input.file <- input$userfile
    if (!is.null(input.file)){
      mytransformeddata <- myupload()
      missingsymbols <- as.character(input$mymissingsymbol)
      missingsymbols <- strsplit(missingsymbols,",")[[1]]
      for( i in 1:length(missingsymbols)){
        mytransformeddata[mytransformeddata == missingsymbols[i]] <- NA
      }
      names(mytransformeddata)[names(mytransformeddata) == input$myphasecolumn] <- "phase.column"
      mytransformeddata <- mytransformeddata[c("phase.column",setdiff(names(mytransformeddata),"phase.column"))]
      
      return(mytransformeddata)
    }  
  })
  
  
  output$varscolumn <- renderUI({
    input.file <- input$userfile
    if (!is.null(input.file)){
      mycolnames <- setdiff(colnames(mydatatransform.selectphase()),"phase.column")
      selectInput(inputId = "myvarscolumn", label = 
                    "Which variable do you want to include in the analysis?",
                  choices = mycolnames)
    }
  })
  
  
  mydatatransform.selectvars <- reactive({
    input.file <- input$userfile
    if (!is.null(input.file)){
      mytransformeddata <- mydatatransform.selectphase()
      
      mytransformeddata <- mytransformeddata[c("phase.column", input$myvarscolumn)]
      return(mytransformeddata)
    }  
  })
  
  output$refphase <- renderUI({
    input.file <- input$userfile
    if (!is.null(input.file)){
      
      myrownames <- as.character(unique(mydatatransform.selectvars()$phase.column))
      selectInput(inputId = "myrefphase", label = 
                    "What is the reference phase?",
                  choices = myrownames)
    }
  })
  
#   output$RCIcheck <- renderUI({
#     input.file <- input$userfile
#     if (!is.null(input.file)){
#       
#       myrownames <- as.character(unique(mydatatransform.selectvars()$phase.column))
#       checkboxInput(inputId = "myRCIcheck", label = 
#                       "Do you want to compute the Reliable Change Index (RCI)?")
#     }
#   })
#   
#   output$RCImethod <- renderUI({
#     input.file <- input$userfile
#     if (!is.null(input.file)){
#       if (input$myRCIcheck){
#         myrownames <- as.character(unique(mydatatransform.selectvars()$phase.column))
#         selectInput(inputId = "myRCIinfo", label = 
#                       "What information is available?",
#                     choices = c("SD of the difference",
#                                 "SD in norm and test-retest reliability"),
#                     selected = "SD of the difference")
#       }
#     }
#   })
  
  
#   output$sddiff <- renderUI({
#     input.file <- input$userfile
#     if (!is.null(input.file)){
#       if (input$myRCIcheck ){
#         if (input$myRCIinfo == "SD of the difference"){
#           numericInput(inputId = "mysddiffinput", label = 
#                          "What is the standard deviation of the difference?", value = 1)
#         }
#       }
#     }
#   })
  
#   output$sdnorm <- renderUI({
#     input.file <- input$userfile
#     if (!is.null(input.file)){
#       if (input$myRCIcheck){
#         if (input$myRCIinfo == "SD in norm and test-retest reliability"){
#           numericInput(inputId = "mysdnorminput", label = 
#                          "What is the standard deviation in the norm group?", value = 1)
#         }
#       }
#     }
#   })
  
#   output$testretestrel <- renderUI({
#     input.file <- input$userfile
#     if (!is.null(input.file)){
#       if (input$myRCIcheck){
#         if (input$myRCIinfo == "SD in norm and test-retest reliability"){
#           numericInput(inputId = "myreliabilityinput", label = 
#                          "What is the test-retest reliability?", value = 0, min = -1, max = 1)
#         }
#       }
#     }
#   })
  
  output$plotButton <- renderUI({
    input.file <- input$userfile
    if (!is.null(input.file)){
      actionButton( inputId = "goButton", label = "Run analysis")
    }
  })
  
  mydatatransform.reorganized <- reactive({
    input.file <- input$userfile
    if (!is.null(input.file)){
      mytransformeddata <- mydatatransform.selectvars()
      char.myrefphase <- as.character(input$myrefphase)
      mytransformeddata$phase.column <- relevel( mytransformeddata$phase.column, ref = char.myrefphase)
      
      varname <- as.character(input$myvarscolumn)
      mytransformeddata$time <- 1:nrow(mytransformeddata) 
      mytransformeddata$id <- 1
      mytransformeddata$timeinphase <- NA
      names(mytransformeddata)[names(mytransformeddata)==varname] <- "y"
      for( i in unique(mytransformeddata$phase.column)){
        mytransformeddata$timeinphase[mytransformeddata$phase.column == i] <-
          (sum(mytransformeddata$phase.column == i)-1):0
      }
      return(mytransformeddata)
    }
  })
  
  generatemodel <- reactive({
    input.file <- input$userfile
    if (!is.null(input.file)){
      mytransformeddata <- mydatatransform.reorganized()
      mymodel <- try(gls( y ~ 1 + phase.column * timeinphase, 
                          correlation = corAR1(form = ~ time | id), data = na.omit(mytransformeddata)))
      if( grepl("glsEstimate", mymodel[[1]])){
        modelcheck <- "Error"; print(modelcheck) } else 
          if( mymodel$apVar[1] == "Non-positive definite approximate variance-covariance") 
          { modelcheck <- mymodel$apVar; print(modelcheck) } else {
            return(mymodel)
          }
    }
  })
  
  
  output$phases <- renderUI({
    input.file <- input$userfile
    if (!is.null(input.file)){
      myrownames <- mydatatransform.reorganized()$phase.column
      checkboxGroupInput(inputId = "myselectedphases", label = 
                           "Which phases do you want to include in the analysis?",
                         choices = myrownames, select = myrownames)
    }
  })
  
#   myRCIresults <- eventReactive(input$goButton, {
#     input.file <- input$userfile
#     if (!is.null(input.file)){
#       if (input$myRCIcheck){
#         if (input$myRCIinfo == "SD of the difference between end of the two phases"){
#           Sdiff <- input$mysddiffinput
#         }
#         if (input$myRCIinfo == "SD in norm and test-retest reliability"){
#           SE <- input$mysdnorminput * (sqrt(1 - input$myreliabilityinput))
#           Sdiff <- sqrt(2 * SE^2)
#         }
#         
#         RCI <- summary(generatemodel())$tTable[2,1] / Sdiff
#         RCI <- ifelse( (abs(RCI) > 1.96), paste0( round(RCI,2), "*"), paste0( round(RCI,2)))
#         return( data.frame( RCI = RCI))
#       }
#     }
#   })
  myresults <- eventReactive(input$goButton, {
    input.file <- input$userfile
    if (!is.null(input.file)){
      mytransformeddata <- mydatatransform.reorganized()
      effectslabels <- rownames(summary(generatemodel())$tTable)
      
      effectslabels[effectslabels == "(Intercept)"] <- paste0("Does ''", input$myvarscolumn,"'' at the end of ''", input$myrefphase, "''-phase differ from zero?")
      effectslabels[ 2: length(unique(mytransformeddata$phase.column))] <- paste0(gsub("phase.column", paste0("Does ''", input$myvarscolumn,"'' at the end of ''", input$myrefphase, "''-phase differ from ''", input$myvarscolumn,"'' at the end of ''"), effectslabels[ 2: length(unique(mytransformeddata$phase.column))]), "''-phase?")
      effectslabels[ length(unique(mytransformeddata$phase.column)) + 1 ] <- paste0("Does time have an effect on ''", input$myvarscolumn, "'' in ''", input$myrefphase, "''-phase?")
      effectslabels[ (length(unique(mytransformeddata$phase.column)) + 2) : length(effectslabels) ] <- gsub("phase.column", "Is there a difference between effect of time in ''", effectslabels[ (length(unique(mytransformeddata$phase.column)) + 2) : length(effectslabels) ])
      effectslabels[ (length(unique(mytransformeddata$phase.column)) + 2) : length(effectslabels) ] <- gsub(":timeinphase", paste0("''-phase and effect of time in ''", input$myrefphase, "''-phase?"), effectslabels[ (length(unique(mytransformeddata$phase.column)) + 2) : length(effectslabels) ])
      
      if( nrow(mytransformeddata) / length(unique(mytransformeddata$phase)) < 6){
        alpha <- 0.01 } else {
          alpha <- 0.05
        }
      
      significants <- (as.data.frame(summary(generatemodel())$tTable)["p-value"] < alpha) * 1
      significants[significants == 1] <- "Yes"
      significants[significants == 0] <- "No"
      colnames(significants) <- "Significant?"
      
      resultstable <- cbind(effectslabels,
                            format(round(summary(generatemodel())$tTable,3),digits = 3),
                            significants)
      resultstable <- rbind(resultstable, c("Carry-over from previous measurement (AR(1) parameter)", format(round(coef(generatemodel()$modelStruct, unconstrained = FALSE),2),digits = 2), rep(NA, 4)))
      resultstable[is.na(resultstable)] <- " "
      colnames(resultstable)[1] <- "Effects"
      colnames(resultstable)[2] <- "Parameter value"
      return(resultstable)
    }
  })
  
  myplot <- eventReactive(input$goButton, {
    input.file <- input$userfile
    if (!is.null(input.file)){
      mytransformeddata <- mydatatransform.reorganized()
      fitted <- fitted(generatemodel())
      varname <- input$myvarscolumn
      theplot <- plot(mytransformeddata$time,
                      mytransformeddata$y, xaxt = "n", xlab = "time", ylab = varname, bty = "n")
      axis(side = 1, at = mytransformeddata$time)
      phasecolor <- 1
      for( phase in unique(mytransformeddata$phase.column)){
        phasecolor <- phasecolor + 1
        points(mytransformeddata$time[mytransformeddata$phase.column == phase], mytransformeddata$y[mytransformeddata$phase.column == phase], pch = 16, cex = 1.5, col = phasecolor)
        lines(mytransformeddata$time[mytransformeddata$phase.column == phase], mytransformeddata$y[mytransformeddata$phase.column == phase], lty = 2, lwd = 3, col = phasecolor)
        lines(as.numeric(names(fitted(generatemodel()))), fitted(generatemodel()), col = "black", lty = 1, lwd = 4)
        
        abline( v = min(mytransformeddata$time[mytransformeddata$phase.column == phase]) - 0.5, lwd = 3)
        abline( v = max(mytransformeddata$time[mytransformeddata$phase.column == phase]) + 0.5, lwd = 3)
      }
      text( quantile(mytransformeddata$time,.75, na.rm = TRUE), quantile(mytransformeddata$y,.90, na.rm = TRUE), "black line = model")
      text( quantile(mytransformeddata$time,.75, na.rm = TRUE), quantile(mytransformeddata$y,.75, na.rm = TRUE), "colored line = data")
      return(theplot)
    }
  })
  
  mydatatransform.reorganized.output <- eventReactive(input$goButton, {
    input.file <- input$userfile
    if (!is.null(input.file)){
      mytransformeddata <- mydatatransform.reorganized()
      return(mytransformeddata)
    }
  })
  
  output$myuploadeddata <- renderDataTable({
    return(myupload())
  }, options = list( info = FALSE, paging = FALSE, ordering = FALSE, searching = FALSE))
  
  output$myreorganizeddata <- renderDataTable({
    return(mydatatransform.reorganized.output())
  }, options = list( info = FALSE, paging = FALSE, ordering = FALSE, searching = FALSE))
  
  output$myresultstable <- renderDataTable({
    return(myresults())
  }, options = list( info = FALSE, paging = FALSE, ordering = FALSE, searching = FALSE))
  
#   output$myRCItable <- renderDataTable({
#     return(myRCIresults())
#   }, options = list( info = FALSE, paging = FALSE, ordering = FALSE, searching = FALSE))
#   
  output$mainOutputPlot <- renderPlot({
    myplot()
  })
  
  output$mystandardtext <- renderText({
    return(infotext())
  })
  
  output$downloadReport <- downloadHandler(
    
    filename = 'my-report.docx',
    content = function(file) {
      src <- normalizePath('mytemplate.Rmd')  
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'mytemplate.Rmd')
      out <- render('mytemplate.Rmd', output_format = word_document())
      file.rename(out, file)  
      
    })
})


ui <- shinyUI(fluidPage(
  #includeCSS("styles.css"),
  
  #   tags$style(type="text/css",
  #              ".shiny-output-error { visibility: hidden; }",
  #              ".shiny-output-error:before { visibility: hidden; }"
  #   ),
  
  # Application title
  titlePanel("E-clip, N = 1 analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    div(class="mysidebarpanel",
        sidebarPanel(
          h3("Input"),
          fileInput(inputId = "userfile",label = "Upload file", accept = ".sav,.csv,.txt,.dat"
          ),
          uiOutput("mycsvInput"),
          textInput(inputId = "mymissingsymbol", value = "9999,99999", label = 
                      "Which symbol(s) used to signify missing values? (separate by comma)"),
          uiOutput("phasecolumn"),
          uiOutput("refphase"),
          uiOutput("varscolumn"),
          uiOutput("participants"),
#           uiOutput("RCIcheck"),
#           uiOutput("RCImethod"),
#           uiOutput("sddiff"),
#           uiOutput("sdnorm"),
#           uiOutput("testretestrel"),
          uiOutput("plotButton"),
          br()#,
          #downloadButton('downloadReport', label = "Download report")
        )),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Output"),
      tabsetPanel( selected = "Uploaded data",
                   tabPanel( title = "Uploaded data",
                             dataTableOutput("myuploadeddata")),
                   tabPanel( title = "Results",
                             h3("Results"),
                             dataTableOutput("myresultstable")),
                             # h3("Reliable Change Index"),
                             # dataTableOutput("myRCItable")),
                   tabPanel( title = "Plot",
                             plotOutput("mainOutputPlot")),
                   tabPanel( title = "Additional information",
                             h3("Assumptions:"),
                             tags$ul(
                               tags$li("the effects of time are linear in both phases;"),
                               tags$li("residuals are normally distributed;"),
                               tags$li("residuals are correlated according to an autoregressive structure.")
                             ),
                             h3("Additional information:"),
                             tags$ul(                     
                               tags$li("If the mean number of data points per group is less than six, an alpha of 0.01 is used to determine whether a result is significant, otherwise, an alpha of 0.05 is used;"
                               )),
                             h3("References"),
                             HTML(paste0("Maric, M., de Haan, E., Hogendoorn, S. M., Wolters, L. H., & Huizenga, H. M. (2014).
                                         Evaluating Statistical and Clinical Significance of Intervention Effects in Single-Case
                                         Experimental Designs: An SPSS Method to Analyze Univariate Data. ", tags$i("Behavior Therapy"), ", ", tags$i("46"), ", 230-241.")),
                             br(),
                             HTML(paste0("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson
                                         (2015). shiny: Web Application Framework for R. R package version
                                         0.12.0.", tags$a(href = "http://CRAN.R-project.org/package=shiny", "http://CRAN.R-project.org/package=shiny"))),
                             br(),
                             HTML(paste0("Pinheiro, J. C., & Bates, D. M. (2000).", tags$i("Mixed-effects models in S and S-PLUS."), " Springer Science & Business Media."))),
                   tabPanel( title = "Reorganized data",
                             dataTableOutput("myreorganizeddata"))
                             )
                             )
                   )))


shinyApp(ui = ui, server = server, options = list( launch.browser = TRUE ))
