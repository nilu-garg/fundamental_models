############################################ FUNDAMENTALS DASHBOARD ###################################################

fluidPage(
  shinyjs::useShinyjs()
  ,theme = shinythemes::shinytheme("yeti")
  ## Title Row ---------------------------------------------------------------------------------------------
    ,fluidRow(
      style = "padding: 20px",
      column(12, align = "center", h2("Earnings Database"))
    )
  
  ,hr(),
  
tabsetPanel(
  
  ###### ADDING DATA --------------------------------------------------------------------
  tabPanel( "Adding New Data", 
            
              fluidRow(style = "padding: 12px",
                      column(12, align = "center", h4("Enter Details for a Company"))),
      
              sidebarLayout(
                ##### SIDEBAR PANEL FOR ADDING DATA -------------------------------------------
                sidebarPanel(
                  
                  wellPanel(
                  selectInput("existingcompanylist","Select the Company:",existingcompanies,selected = "8x8")
                  ,selectInput("listofmetrics","Select the metric to update:",metricslist,selected = metricslist[1])
                  ,column(12, align = "center", actionButton("submit1",'Show Table'))
                  )
                
                  ,wellPanel(
                    
                  checkboxInput("newmetric", "Add a new metric to the selected Company", FALSE)
                  
                  ,conditionalPanel( 
                    condition = "input.newmetric == true",
                    textInput("metricNameText2","Enter Metric Name:"),
                    textInput("denoText2","Enter Metric Denomination eg($M,#K):"),
                    textInput("startingQuarterLabelText2","Enter Starting Quarter Label eg('Q1-Y2018'):"),
                    dateInput("startingQuarterText2","Enter Last Day of First Quarter(eg, 2019-03-31 for the end of the 1st quarter):")
                    ,column(12, align = "center", actionButton("submit2",'Create Table'))
                    ,br()
                  )

                  
                    ,checkboxInput("newcompany", "Add a new company", FALSE)
                    
                    ,conditionalPanel( 
                      condition = "input.newcompany == true",
                      textInput("companyNameText3", "Enter Company Name:"),
                      textInput("companyTickerText3", "Enter Company Ticker:"),
                      textInput("companyNoteText3", "Enter Company Notes/Description:"),
                      textInput("metricNameText3","Enter Metric Name:"),
                      textInput("denoText3","Enter Metric Denomination eg($M,#K):"),
                      textInput("startingQuarterLabelText3","Enter Starting Quarter Label eg('Q1-Y2018'):")
                      ,dateInput("startingQuarterText3","Enter Last Day of First Quarter(eg, 3/31/20 for the end of the 1st quarter):")
                      ,column(12, align = "center", actionButton("submit3",'Create Table'))
                    )
                    
                  )

                ),
                #### MAIN PANEL FOR ADDING DATA --------------------------------------------------------
                mainPanel(
                  tags$head(
                    tags$style(type='text/css'
                               , "table.data { width: 300px; }"
                               , ".well {width: 80%; background-color: NULL; border: 0px solid rgb(255, 255, 255); box-shadow: 0px 0px 0px rgb(255, 255, 255) inset;}"
                               , ".tableinput .hide {display: table-header-group; color: black; align-items: center; text-align: center; align-self: center;}"
                               , ".tableinput-container {width: 100%; text-align: center;}"
                               , ".tableinput-buttons {margin: 10px;}"
                               , ".data {background-color: rgb(255,255,255);}"
                               , ".table th, .table td {text-align: center;}"
                               ))
                    
                   
                  ,fluidRow(
                    column(4,uiOutput("matrix.input"))
                    ,hr()
                    ,column(8,uiOutput("matrix.output")) 
                  )
                  )
                )
            )
  #---------------------------------------------------------------------------------------------------
  
  ####### DOWNLOADING DATA -------------------------------------------------------
  
  ,tabPanel("Downloading Existing Data"
           ,fluidRow(style = "padding: 12px",
                    column(12, align = "center", h4("Enter Details for a Company")))
           
           ,wellPanel(
             fluidRow(
               column(8,align = "left",selectInput("downloadcomapnyname","Select the Company:",existingcompanies,selected = "8x8"))
               ,column(4, align = "center", downloadButton("downloadtable",'Download Table'))
             )
             
           )
           ,fluidRow(
             column(12,align = "center",textOutput("company.info")),
             br(),
             hr(),
             tableOutput('table.download')
           )
           )
  
  ####### EARNINGS DATA -------------------------------------------------------
  
  ,tabPanel(
    "Store Report Dates"
    ,sidebarLayout(
     sidebarPanel(
       wellPanel(
         selectInput("EarningsComapnyName","Select the Company:",existingcompanies,selected = "8x8")
         ,textInput("EarningsQuarterLabel","Enter Quarter Label eg('Q1-Y2018'):")
         ,dateInput("EarningsDate","Enter the report date YYYY-MM-DD format")
         ,column(12, align = "center", actionButton("submit4",'Store in Database'))
         ,br()
       )
       ,fluidRow(
         column(8,uiOutput("earnings.output"))
       )
     )
     
     ,mainPanel(
       tableOutput("earnings")
       
     )
    )
    
    
  )
  
  ####### CONSENSUS DATA -------------------------------------------------------
  
  ,tabPanel(
    "Store Consensus Data"
    ,sidebarLayout(
      sidebarPanel(
        wellPanel(
          column(12,align = "left",selectInput("ConsensusComapnyName","Select the Company:",existingcompanies,selected = "8x8"))
          ,column(12,align = "left",selectInput("consensusmetrics","Select the Consensus related metric:",metricslist,selected = metricslist[1]))
          ,column(12,align = "left",textInput("ConsensusQuarterLabel","Enter Quarter Label eg('Q1-Y2018'):"))
          ,column(12,align = "left",numericInput("Consensus","Enter the Consensus eg 30.6 for 30.6%:",value = 0))
          ,column(12, align = "center", actionButton("submit5",'Store in Database'))
          ,br()
        )
        ,fluidRow(
          column(8,uiOutput("consensus.output"))
        )
      )
      
      ,mainPanel(
        tableOutput("consensus")
        
      )
    )
    
    
  )
  

#########################################################################
))


 