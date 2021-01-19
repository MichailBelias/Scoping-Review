library(shinyswipr)
library(readxl)
library(foreign)
library(readr)
library(haven)
library(readxl)
library(writexl)
library(xlsx)

#df <- read_excel("Original Data-set/Michael.xlsx", sheet = 1)
#df = df[-c(972:3700),]
#df = df[which(is.na(df$`Include (Yes/No/Unclear)`)),]
#write_xlsx(df, "Michael.xlsx")

df <- read.xlsx("IPD-MA (full).xlsx", sheetIndex = 1)
df=  df[is.na(df$Michael),]
df$Count.number =  as.character(df$Count.number) ; 
df =  df[order(df$Count.number,decreasing = T),]


#  df <- read.csv("csv-individual-set_till_2020.csv")
#App is a simple card with some content and a little output below that represents the last swipes result.

# devtools::install_github("nstrayer/shinyswipr")
library(shinyswipr)
library(shiny)



#######
ui <- shinyUI(pageWithSidebar(
  
  # Header:
  headerPanel("R data reader"),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$style(type='text/css', ".well { max-width: 20em; }"),
    # Tags:
    tags$head(
      tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
      tags$style(type="text/css", "select { width: 100%}"),
      tags$style(type="text/css", "input { width: 19em; max-width:100%}")
    ),
    
    # Select filetype:
    selectInput("readFunction", "Function to read data:", c(
      # Base R:
      "read_sav",
      "read.table",
      "read.csv",
      "read.csv2",
      "read.delim",
      "read.delim2",
      
      # foreign functions:
      "read.arff",
      "read.dbf",
      "read.dta",
      "read.epiiinfo",
      "read.mtp",
      "read.octave",
      "read.ssd",
      "read.systat",
      "read.xport",
      
      # Advanced functions:
      "read.xlsx",
      "read.xlsx2",
      "read_excel",
      "scan",
      "readLines"
    )),
    
    # Argument selecter:
    htmlOutput("ArgSelect"),
    # Argument field:
    htmlOutput("ArgText"),
    
    
    # Upload data:
    fileInput("file", "Upload data-file:"),
    
    # Variable selection:
    htmlOutput("varselect"),
    
    textInput("name","Dataset name:","Data"),
    
    br(),
    
    # Select filetype:
    selectInput("write.Function", "Function to read data:", c(
      # Base R:
      "write_sav",
      "write.table",
      "write.csv",
      "write.csv2",
      "write.delim",
      "write.delim2",
      
      # foreign functions:
      "write.arff",
      "write.dbf",
      "write.dta",
      "write.epiiinfo",
      "write.mtp",
      "write.octave",
      "write.ssd",
      "write.systat",
      "write.xport",
      
      # Advanced functions:
      "write.xlsx",
      "write.xlsx2",
      "scan",
      "readLines"
    )),
    # write.Argument selecter:
    htmlOutput("write.ArgSelect"),
    # write.Argument field:
    htmlOutput("write.ArgText"),
    sliderInput("number.of.observations", "observations to show" ,min = 1 , max=100, value = 10)
    
  ),  
  # Main:
  mainPanel(
    h1("Systematic Review on metrics"),
    tags$b("Aim:"),
    p("We are searching for indicators or metrics capturing the quality, efficiency, or relevance of whole trials from a 
  system level (e.g. proportion of trials with prospective registration, proportion of trials recruiting to target, proportion 
  of trials with published results, etc.) that have been suggested in the literature or already used in empirical studies to 
  monitor cohorts/portfolios of clinical trials for this purpose."),
    tags$b("Inclusion criteria"),
    tags$ol(
      tags$li("The authors imply that the quality or relevance or efficiency of a cohort/sample of clinical trials was assessed in any way 
(e.g. Improving the relevance of randomised trials…) OR (Improving clinical trial quality through…)"),
      tags$li("Articles which defined or suggested at least one quantifiable indicator for clinical trials to assess or monitor 
  quality relevance efficiency (e.g. expert group listing relevant indicators)")),
    tags$b("Exclusion criteria"),
    tags$ol(
      tags$li("If a specific indicator is only presented in the context of trial site performance (instead of whole trials)"),
      tags$li("Articles - presenting indicators which are undoubtedly only for a specific sub-group of clinical trials relevant (e.g. certain measurements, standards, 
or technologies used in clinical trials; imaging metrics for instance)"), 
      tags$li("If authors explicitly state that specific indicators are not or only of limited relevance for clinical trial quality/efficiency 
(recommendation against certain indicators)"),
      tags$li("Articles assessing only adherence to reporting guidelines (e.g. CONSORT or SPIRIT)"),
      tags$li("Articles assessing only risk of bias of trials")),
    p("Swipe to each direction (up, down, left, right). Down: Definitely include, Up: Definitely exclude, Right: Probably include, Left: Probably exclude"),
    hr(),
    # Button
    downloadLink("downloadData", "Download"),
    shinyswiprUI( "quote_swiper",
                  h4("Swipe Me!"),
                  hr(),
                  h4("ID number"),
                  textOutput("quote_ID"),
                  h4("Title"),
                  textOutput("quote_record"),
                  h4("Abstract"),
                  textOutput("quote"),
                  h4("Author (s) :"),
                  textOutput("quote_author")
    ),
    hr(),
    h4("Swipe History"),
    tableOutput("resultsTable")
  )))
#######




server <- function(input, output, session) {
  #Importing Data Code#
  ######### 
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ### write Argument names:
  write.ArgNames <- reactive({
    write.Names <- names(formals(input$write.Function)[-1])
    write.Names <- write.Names[write.Names!="..."]
    return(write.Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  # Argument selector:
  output$write.ArgSelect <- renderUI({
    if (length(write.ArgNames())==0) return(NULL)
    
    selectInput("write.arg","Argument:",write.ArgNames())
  })
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
  
  ## write.Arg text field:
  output$write.ArgText <- renderUI({
    write.fun__arg <- paste0(input$write.Function,"__",input$write.arg)
    
    if (is.null(input$write.arg)) return(NULL)
    
    write.Defaults <- formals(input$write.Function)
    
    if (is.null(input[[write.fun__arg]]))
    {
      textInput(write.fun__arg, label = "Enter value:", value = deparse(write.Defaults[[input$write.arg]])) 
    } else {
      textInput(write.fun__arg, label = "Enter value:", value = input[[write.fun__arg]]) 
    }
  })
  
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  
  # Select variables:
  output$varselect <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput("vars", "Variables to use:", names(Dataset()), multiple =TRUE)            
  })
  
  ### Download dump:
  
  output$downloadDump <- downloadHandler(
    filename = "Rdata.R",
    content = function(con) {
      
      assign(input$name, Dataset()[,input$vars,drop=FALSE])
      
      dump(input$name, con)
    }
  )
  
  ### Download save:
  
  output$downloadSave <- downloadHandler(
    filename = "Rdata.RData",
    content = function(con) {
      
      assign(input$name, Dataset()[,input$vars,drop=FALSE])
      
      save(list=input$name, file=con)
    }
  )
  
  
  i <- 1
  
  
  # Initiate the swiping 
  card_swipe <- callModule(shinyswipr, "quote_swiper")
  
  quote               <- as.list(df[i,])
  
  output$quote_ID        <- renderText({ as.character(quote$`Count.number` )})
  output$quote        <- renderText({ as.character(quote$`Abstract` )})
  output$quote_author <- renderText({ as.character(quote$Author) })
  output$quote_record <- renderText({ as.character(appVals$quote$Title)})
  # Create a new data-set that has the swiped
  
  appVals <- reactiveValues(
    quote  = quote,
    swipes = data.frame(Title = character(),
                        `Abstract` = character(), 
                        Author = character(), 
                        swipe = character())
  )
  
  output$resultsTable <- renderTable({appVals$swipes})
  
  observeEvent( card_swipe(),{
    
    i<<- i + 1
    #Record our last swipe results.
    appVals$swipes <- rbind(appVals$swipes,
                            data.frame( 'Record number'  = appVals$quote$Title,
                                        'Abstract'  = appVals$quote$`Abstract`,
                                        'Author' = appVals$quote$Author,
                                        swipe  = card_swipe()
                            )
    )
    #send results to the output.
    output$resultsTable <- renderTable({appVals$swipes})
    
    #update the quote
    appVals$quote <- as.list(df[i,])
    
    #send update to the ui.
    output$quote_ID <- renderText({ as.character(appVals$quote$`Count.number` )})
    output$quote_record <- renderText({ as.character(appVals$quote$Title )})
    output$quote <- renderText({ as.character(appVals$quote$`Abstract` )})
    output$quote_author <- renderText({ as.character(appVals$quote$Author) })
    
    # Save Inputs
    write.csv(appVals$swipes, file = "Michael.csv", row.names=FALSE )
    
    df3 =  cbind(df[-c(1:i-1),])
    df2 =  rbind(cbind(df[c(1:i-1),], swipe =  appVals$swipes$swipe), 
                 cbind(df[-c(1:i-1),],swipe = NA))
    
    
    write_xlsx(x = df3, "Michael.xlsx")
    write_xlsx(x = df2, "Michael2.xlsx")
  }) #close event observe.
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      df2 =  rbind(cbind(df[c(1:i-1),], swipe =  appVals$swipes$swipe), 
                   cbind(df[-c(1:i-1),],swipe = NA))
      write_xlsx(x = df2, file)
    }
  )
  
  
}

shinyApp(ui, server)