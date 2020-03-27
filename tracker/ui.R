
################################### FORWARD TRACKING FUNDAMENTAL MODELS ###################################

fluidPage(
  theme = shinythemes::shinytheme("united"),
  
  ## Title Row ---------------------------------------------------------------------------------------------
  tabsetPanel(
    
    ## TAB 1 -----------------------------------------------------------------------------------------
    tabPanel( "Checking if Database is Updated"
              ,fluidRow(
                style = "padding: 5px"
                ,column(12, align = "center", h4("Make sure current month's data is in the database"))
              )
              ,hr()
              ,wellPanel(
              fluidRow( style = "padding: 12px"
                ,column(6,align = "left",actionButton('submit1','Check for data'))
              )
              ,fluidRow(column(12,align = "center",dataTableOutput('missing_data')))
                
              )
      
    )
    ## TAB 2 -----------------------------------------------------------------------------------------
    ,tabPanel( "Estimating Values for New Quarter"
              ,fluidRow(
                style = "padding: 5px"
                ,column(12, align = "center", h4("Enter the most recent quarter's end date and get estiamtes for all forward tracked models"))
              )
              ,hr()
              ,wellPanel(
              fluidRow(
                style = "padding: 12px"
                ,column(6,align = "left",dateInput("current_ending_quarter","Enter the end date of current quarter:"))
                ,column(6,align = "left",actionButton('submit2','Submit')
                        ,downloadButton("new_quarter_download","Download Data"))
              )
     
                ,fluidRow(style = "padding: 12px"
                ,column(12,align = "center",textOutput('total_model_toupdate'))
              )
              ,fluidRow(column(12,align = "center",dataTableOutput('client_file_estimate'))
              ))
      
    )
    ## TAB 3 -----------------------------------------------------------------------------------------
    ,tabPanel( "Getting Monthly Data for Client Models"
               ,fluidRow(
                 style = "padding: 5px"
                 ,column(12, align = "center", h4("Outputs a file with monthly data for all the client models"))
               )
               ,hr()
               ,wellPanel(
               fluidRow(
                 style = "padding: 12px"
                 ,column(6,align = "left",selectInput('mon_track_companyname','Companies:',c("SaaS","Consumer","Financial","Healthcare","Utilities")))
                 ,br()
                 ,column(6,align = "left",actionButton('submit3','Submit')
                         ,downloadButton("mon_tracker_download","Download Data"))
               )
                  ,fluidRow(column(12,align = "center",dataTableOutput('mon_tracker_df'))
               ))
      
    )
    ## TAB 4 -----------------------------------------------------------------------------------------
    ,tabPanel( "Updating Individual Ticker Models"
               ,fluidRow(
                 style = "padding: 5px"
                 ,column(12, align = "center", h4("Upudates the model after a company reports"))
               )
               ,hr()
               ,wellPanel(
                fluidRow(
                 style = "padding: 12px"
                 ,column(3,align = "left",selectInput("company_ticker","Select a Ticker",unique(companyids$Ticker)))
                 ,column(3,align = "center",actionButton('submit4','Submit'))
                 
               )
               ,fluidRow(column(12,align = "center",tableOutput('model_status')))
               ,fluidRow(column(12,align = "center",tableOutput('model_error_rate')))
               
               )
    )
    
    # tabsetPanel ----------------------
  )
  # fluid page ----------------------------
)
