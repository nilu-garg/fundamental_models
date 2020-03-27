
################################### GENERATING MODELS ###################################

fluidPage(
  theme = shinythemes::shinytheme("simplex"),
  
  ## Title Row ---------------------------------------------------------------------------------------------
  fluidRow(
    style = "padding: 20px",
    column(12, align = "center", h2("Generating Models"))
  ),
  
  hr(),
  
  ## Creating Tabs ---------------------------------------------------------------------------------------------
  
  tabsetPanel(
    
  ## Check if data is in the database ---------------------------------------------------------------------------------------------
    
    tabPanel("Review strat database",fluid = T
             ,fluidRow(
               style = "padding: 5px",
               column(12, align = "center", h4("Check if SimilarWeb data is in the database"))
             )
             ,hr()
             ,wellPanel(
               fluidRow(
                 style = "padding: 5px"
                 ,column(6,align = "left",textInput("companydomain", "Company Domain:"))
                 ,column(6,align = "left",actionButton('submit1','Submit'))
               )
               ,fluidRow(column(12,align = "center",tableOutput('traff_db')))
               ,fluidRow(column(12,align = "center",tableOutput('pp_db')))
               )
              )
    
  ## Explore the domain ---------------------------------------------------------------------------------------------
    
    ,tabPanel( "Explore Popular Pages",fluid = T
              ,fluidRow(style = "padding: 5px",
                column(12, align = "center", h4("Explore keywords and frequency for a domain"))
              )
              ,hr()
              ,wellPanel(
                fluidRow( style = "padding: 5px"
                          ,column(6,align = "left",textInput("exploredomain", "Enter Domain:"))
                          ,column(6,align = "left",actionButton('submit2','Submit'))
                )
                ,fluidRow(style = "padding: 5px"
                          ,column(4,align = "center",dataTableOutput('subdomaindf'))
                          ,column(4,align = "center",dataTableOutput('keyworddf'))
                          ,column(4,align = "center",dataTableOutput('ppdf'))
                          )
                        )
            ) 
    
  ## Creating Models -----------------------------------------------------------------------------------------------
    ,tabPanel( "Create models",fluid = T
               ,fluidRow(style = "padding: 5px",
                         column(12, align = "center", h4("Enter the ticker and the parameters to create the models"))
               )
               ,hr()
               ,wellPanel(
                 fluidRow(style = "padding: 5px"
                          ,column(2,align = "center",textInput("companyticker","Enter Ticker:"))
                          ,column(6,align = "center",radioButtons('model_creation_type',"Choose model type",c("Looping through subdomains" = "subdomainloop",
                                                                                                              "Looping through keywords" = "keywordloop"),inline = T))
                          )
                          ,conditionalPanel(
                            condition = "input.model_creation_type == 'subdomainloop'"
                            ,fluidRow(style = "padding: 5px"
                              ,column(6,align = "center",textInput("domainlist","Enter list of subdomains"))
                            ,column(6,align = "center",actionButton('submit3','Submit'))
                            )
                            ,fluidRow(style = "padding: 5px"
                              ,column(12,align = "center",dataTableOutput('subdomainloopresults'))
                              )
                          )
                          ,conditionalPanel(
                            condition = "input.model_creation_type == 'keywordloop'"
                            ,fluidRow(style = "padding: 5px"
                            ,column(4,align = "center",textInput("keyworddomain","Enter domain"))
                            ,column(4,align = "center",textInput("keywordlist","Enter list of keywords"))
                            ,column(4,align = "center",actionButton('submit4','Submit'))
                            )
                            ,fluidRow(style = "padding: 5px"
                                      ,column(12,align = "center",dataTableOutput('keywordloopresults'))
                            )
                          )
                   
                 )
               
      
    )
    
  ## Visulizing models and Pushing data to SQL ----------------------------------------------------------------------------------------------
    
    ,tabPanel( "Visualize Models",fluid = T
               ,fluidRow(style = "padding: 5px",
                         column(12, align = "center", h4("Visulize the model graphs and add it to the database"))
               )
               ,hr()
               ,wellPanel(
                 fluidRow(style = "padding: 5px"
                    ,column(4,align = "center",textInput('modelticker',"Enter model ticker:"))
                    ,column(4,align = "center",textInput('model_config_id',"Enter results model id:"))
                    ,column(4,align = "center",actionButton("submit5","Visulize the model"))
                    
                 ))
                ,wellPanel(               
                 fluidRow(style = "padding: 5px"
                    ,column(1,align = "center",radioButtons("for_client","For Clients",c(1,0)))
                    ,column(1,align = "center",radioButtons("forward_tracked","Is Forward Tracked",c(1,0)))
                    ,column(1,align = "center",selectInput("category","Select Category",c("SaaS","Consumer","Financial","Healthcare","Telecom","Industrials","Utilities")))
                    ,column(3,align = "center",textInput("modellogic","Enter Logic","Traffic encompassing overall site activity"))
                    ,column(2,align = "center",textInput('model_config_id_final',"Enter the model_config_id for database"))
                    ,column(2,align = "center",actionButton("submit6","Push the model to db"))
                    ,column(2,align = "center",textOutput("databasestatus"))
                    
                 ))
                ,wellPanel(               
                 fluidRow(style = "padding: 5px"
                  ,column(6,plotOutput('regxyyoy', height = "350px"))
                  ,column(6,plotOutput('regxylevel', height = "350px"))
                 )
                 ,fluidRow(style = "padding: 5px"
                  ,column(6,plotOutput('yoyxy',height = "350px"))
                  ,column(6,plotOutput('levelxy',height = "350px"))
                 )
               )
      
    )
      
    
  ## tabset panel -------------------------------
  )
  ## fluid page ----------------------------
)
