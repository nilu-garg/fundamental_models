################################### FUNDAMENTAL MODELS VIEWING ###################################

fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  
  ## Title Row ---------------------------------------------------------------------------------------------
  fluidRow(
    style = "padding: 20px",
    column(12, align = "center", h2("Exploring New Models"))
  ),
  
  hr(),
  
  ## Creating Tabs ---------------------------------------------------------------------------------------------
  
  tabsetPanel(
    ## Tab 1 ---------------------------------------------------------------------
    tabPanel("Summary File", fluid = T
             ,fluidRow(
               style = "padding: 5px",
               column(12, align = "center", h4("Current Standing of all models"))
             )
             ,hr()
          ,wellPanel(
            fluidRow(style = "padding: 5px"
              ,column(3,align = 'center',selectInput('summary_category','Select a category',categories_name))
              ,column(3,align = 'center',actionButton('submit1','Submit'),downloadButton("summary_file_download","Download Data"))
            )
            ,fluidRow(column(12,align = "center",dataTableOutput('summary_table')))
          )
          
    )
    ## Tab 2 ---------------------------------------------------------------------
    ,tabPanel("Model Graphs", fluid = T
             ,fluidRow(
               style = "padding: 5px",
               column(12, align = "center", h4("Regresssion graphs for the selected model"))
             )
             ,hr()
             ,wellPanel(
               fluidRow(style = "padding: 5px"
                        ,column(3,align = 'center',selectInput('select_ticker','Select a ticker',model_tickers$Ticker_Name))
                        ,column(3,align = 'center',selectInput('select_model_config_id','Select a model','model_config_id'))
                        ,column(3,align = 'center',actionButton('submit2','Submit'),downloadButton("raw_data_download","Download Data"))
               )
               ,fluidRow(column(12,align = "center",tableOutput('model_specification')))
               ,fluidRow(column(12,align = "center",tableOutput('model_domain_list')))
               ,fluidRow(column(12,align = "center",tableOutput('model_error_rate')))
               ,fluidRow(style = "padding: 5px"
                          ,column(6,plotOutput('regxyyoy', height = "350px"))
                          ,column(6,plotOutput('regxylevel', height = "350px")))
               ,fluidRow(style = "padding: 5px"
                           ,column(6,plotOutput('yoyxy',height = "350px"))
                           ,column(6,plotOutput('levelxy',height = "350px")))
               ,fluidRow(style = "padding: 5px",column(12,align = "center",tableOutput('model_raw_data')))
             )
             
    )
    ## Tab 3 ---------------------------------------------------------------------
    ,tabPanel("Historical Estimates",fluid = T
              ,fluidRow(
                style = "padding: 5px",
                column(12, align = "center", h4("List of forecast estimates and errors"))
              )
              ,hr()
              ,wellPanel(
                checkboxInput("quarter_error","See all estimates and errors for a Quarter",TRUE)
                ,conditionalPanel( condition = "input.quarter_error == true"
                                  ,fluidRow( style = "padding: 5px"
                                            ,column(3,align = 'center',dateInput('select_date_error','Select a quarter'))
                                            ,column(3,align = 'center',actionButton('submit3','Submit')))                 
                                 )
                ,conditionalPanel(condition = "input.quarter_error == false"
                                  ,fluidRow( style = "padding: 5px"
                                             ,column(3,align = 'center',selectInput('select_ticker_error',
                                                                                    'Select a ticker',model_tickers$Ticker_Name))
                                             ,column(3,align = 'center',actionButton('submit4','Submit')))
                                 )
                ,fluidRow(column(12,align = "center",tableOutput('error_summary')))
                ,fluidRow(column(12,align = "center",dataTableOutput('error_table')))
                       )
            )    
   
    ## TAB 4 -----------------------------------------------------------------------------------------
  ,tabPanel( "Creating Report Card"
             ,fluidRow(
               style = "padding: 5px"
               ,column(12, align = "center", h4("Calculates error rates for sector wise models"))
             )
             ,hr()
             ,wellPanel(
               fluidRow(
                 style = "padding: 5px"
                 ,column(3,align = "left",selectInput("industry_category","Select Industry Category",categories_name))
                 ,column(6,align = "left",actionButton('submit5','Submit')
                         ,downloadButton("models_report_card_download","Download Data"))
               )
               ,fluidRow(column(12,align = "center",dataTableOutput('report_card')))
             )
  )
  
  
  ## tabset panel -------------------------------  
  )
## fluid page ------------------------------------------  
)