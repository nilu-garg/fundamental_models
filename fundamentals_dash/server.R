############################################ FUNDAMENTALS DASHBOARD ###################################################

shinyServer(
  function(input, output,session) {
    
    ##################################################################
    #### UPDATE EXISTING REPORTED METRIC ####
    ##################################################################
   
    observe({
      
      companyName <- input$existingcompanylist
      
      companyid <- get_comapny_id(con,companyName,"")
      
      dbmetricslist <- get_metric_list(con,companyid)
      
      if(class(dbmetricslist)=="data.frame"){
        metricslist <- dbmetricslist$Metric
        updateSelectInput(session,"listofmetrics",
                          choices = metricslist)
      }else{
        updateSelectInput(session,"listofmetrics",
                          choices = "nometrics")
      }
      existingcompanylist <- sort(get_all_company_list(con)$Name)
      updateSelectInput(session,'existingcompanylist',choices = existingcompanylist,selected = companyName)
      updateSelectInput(session,'downloadcomapnyname',choices = existingcompanylist)
      updateSelectInput(session,'EarningsComapnyName',choices = existingcompanylist,selected = existingcompanylist[1])
      updateSelectInput(session,'ConsensusComapnyName',choices = existingcompanylist,selected = "8X8")
      
    })
    
    quartervalues <- reactiveValues()  
    
    observeEvent(input$submit1,{
      
      companyName <- input$existingcompanylist
      metricName <- input$listofmetrics

      companyid <- get_comapny_id(con,companyName,"")
      dbmetricslist = get_metric_list(con,companyid)
      metricid <- dbmetricslist[dbmetricslist$Metric==metricName,]$id
      
      dbreportedvalues <- get_reported_metrics(con,companyid,metricid)
      dbreportedvalues <- dbreportedvalues[order(dbreportedvalues$Start_date),]
      
      if(dbreportedvalues!="No Reported Metric for selected company and metric"){
        startQuarter <- dbreportedvalues$Start_date[nrow(dbreportedvalues)]
        startQuarter <- date(startQuarter)
        startQuarterLabel <- dbreportedvalues$Quarter[nrow(dbreportedvalues)]
        currentMonth <- as.Date(cut(Sys.Date(), "month"))
        
        dbreportedvalues$Start_date <- as.character(as.Date(dbreportedvalues$Start_date))
        dbreportedvalues$End_date <- as.character(as.Date(dbreportedvalues$End_date))
        output$table.output <- renderTable({ dbreportedvalues[,c("End_date","Quarter","Reported_value")]}
                                           , include.rownames = FALSE
                                           , include.colnames = TRUE
        )
        quartervalues$startQuarterLabel <- startQuarterLabel
        quartervalues$startQuarter <- startQuarter

        if(str_detect(startQuarterLabel,"^Q[1-4]{1}-Y[0-9]{4}") & startQuarter<currentMonth & metricName!=""){
          
          quarterStartDateSeries <- seq(startQuarter,currentMonth,by = "3 months")
          if(length(quarterStartDateSeries)==1){
            qtostart = startQuarterLabel
          }else{
            qtostart <- create_quarterlabel_list(startQuarterLabel,length(quarterStartDateSeries))
          }
          quarterEndDateSeries <- create_quarter_end(quarterStartDateSeries)
          quartervalues$quarterStartDateSeries <- quarterStartDateSeries
          quartervalues$quarterEndDateSeries <- quarterEndDateSeries
          quartervalues$qtostart <- qtostart
          
          if(length(quarterEndDateSeries)==1){
            df <- data.frame("No new quarters available")
          }else{
            df <- create_empty_matrixdf(as.character(quarterEndDateSeries),qtostart,metricName)
            df <- df[2:nrow(df),]
          }
         
          
          output$matrix.input <- renderUI(
            fluidRow(
              h5(paste0("Data already reported for ",metricName))
              ,tableOutput('table.output')
              ,matrixInput(inputId = 'data1',value = as.matrix(df),class = "character")
              ,column(12, align = "center", actionButton("updateexistingcompany",'Store Table'))
            )
          )
          output$matrix.output <- renderUI(
            wellPanel(h5("Waiting for you to fill the table"))
          )
        }else{
          output$matrix.input <- renderUI(
            h5("Check Metric Name, Quarter Label, Start Date")
          )
          output$matrix.output <- renderUI(
            wellPanel(h5("Waiting for you to fill the table"))
          )
        }
        
        
      }else{output$matrix.input <- renderUI(
        h5("No initial data for this metric, check the add new metric")
      )
      output$matrix.output <- renderUI(
        wellPanel(h5("Waiting for you to fill the table"))
      )
        
      }
      

      })
    
    reportedvalues <- reactiveValues()

    #### FINALISE THE NEW REPORTED VALUES FOR AN EXISTING COMPANY_METRIC_REPORTED 
    
    observeEvent(input$updateexistingcompany, {
      companyName <- input$existingcompanylist
      metricName <- input$listofmetrics

      startQuarterLabel <- quartervalues$startQuarterLabel
      startQuarter <- quartervalues$startQuarter
      
      metricdf <- data.frame(input$data1)
      metricdf[,3] <- as.numeric(as.character(metricdf[,3]))
      quarterStartDateSeries <- quartervalues$quarterStartDateSeries
      metricdf$Start <- as.character(quarterStartDateSeries[2:length(quarterStartDateSeries)])
      metricdf <- metricdf %>% filter(!is.na(X3))
      colnames(metricdf) <- c("End","QuarterLabel",metricName,"Start")
       output$table.output1 <- renderTable({ metricdf[,c(2,4,1,3)]}
                                         , include.rownames = FALSE
                                         , include.colnames = TRUE
      ) 
      
      output$exisitngcompanydetails <- renderUI({
        str1 <- paste0( "Company Name Entered: ",companyName) 
        str2 <- paste0("Company Metric: ",metricName)
        HTML(paste(str1, str2, sep = '<br/>'))
      })
      
      output$matrix.output <- renderUI({
        fluidRow(
          h5("Information Entered:")
          ,htmlOutput("exisitngcompanydetails")
          ,br()
          ,tableOutput('table.output1')
          ,h5("The above table will go into the database. Change anything in the table beside or Click the update button to store it")
          ,column(12, align = "center", actionButton("updatetable1",'Update Database Table'))
        )
      })
      
      reportedvalues$name <- companyName
      reportedvalues$metric <- metricName
      reportedvalues$metrictable <- metricdf

    }) 
    
    #### STORE THE NEW QUARTER VALUES INTO THE DATABASE 
    
    observeEvent(input$updatetable1, {
      
      valuestr <- "Values Database Error"
      tryCatch({
        
        companyid <- get_comapny_id(con,reportedvalues$name,"")
     
        dbmetricslist = get_metric_list(con,companyid)

        metricid <- dbmetricslist[dbmetricslist$Metric==reportedvalues$metric,]$id[1]

        df <- reportedvalues$metrictable
        df$Company_id <- companyid
        df$Metric_id <- metricid

        toupload <- df[,c(5,6,2,4,1,3)]
        colnames(toupload) <- c("Company_id","Metric_id","Quarter","Start_date","End_date","Reported_value")
        toupload <- toupload %>% filter(!is.na(Reported_value))
        print(toupload)
        
        valuestr <- write_database(toupload,"Reported",con)
        
      },error = function(err){ print(paste0("Reported Values Already Exists",err) )  })
      
      output$matrix.input <- renderUI({})
      output$matrix.output <- renderUI({
        fluidRow(
          h5("Database updated with the values")
          ,h5(valuestr)
          
        )})
    },label = "finalupdate1",autoDestroy=TRUE)
    
    ##################################################################
    #### FOR A NEW COMPANY ####
    ##################################################################
    
    observeEvent(input$submit3,{
      
      companyName3 <- input$companyNameText3
      ticker3 <- input$companyTickerText3
      companyNote3 <- input$companyNoteText3 
      metricName3 <- input$metricNameText3
      metricNote3 <- input$denoText3
      startQuarterLabel3 <- input$startingQuarterLabelText3
      startQuarterLastday3 <- input$startingQuarterText3
      currentMonth3 <- as.Date(cut(Sys.Date(), "month"))
      
      startQuarter3 <- startQuarterLastday3 - 5
      month(startQuarter3) <- month(as.Date(startQuarter3)) - 2
      startQuarter3 <- as.Date(cut(startQuarter3,"month"))
      
      if(str_detect(startQuarterLabel3,"^Q[1-4]{1}-Y[0-9]{4}") & startQuarter3<currentMonth3 & metricName3!=""){
        
        quarterStartDateSeries3 <- seq(startQuarter3,currentMonth3,by = "3 months")
        if(length(quarterStartDateSeries3)==1){
          qtostart3 <- startQuarterLabel3
        }else{
          qtostart3 <- create_quarterlabel_list(startQuarterLabel3,length(quarterStartDateSeries3))
        }
        quarterEndDateSeries3 <- create_quarter_end(quarterStartDateSeries3)
        df3 <- create_empty_matrixdf(quarterEndDateSeries3,qtostart3,metricName3)
        output$matrix.input <- renderUI(
          fluidRow(
            h5(paste0("Input Table"))
            ,matrixInput(inputId = 'data3', value = as.matrix(df3),class = "character")
            ,column(12, align = "center", actionButton("storenewcompany",'Store Table'))
        ))
        
        output$matrix.output <- renderUI(
          h5("Waiting for you to fill the table")
        )
      }else{
        output$matrix.input <- renderUI(
          h5("Check Metric Name, Quarter Label, Start Date")
        )
        output$matrix.output <- renderUI(
          h5("Waiting for you to fill the table")
        )
      }
  
    })
    
    companyreactive <- reactiveValues()
    
    #### FINALISE THE NEW REPORTED VALUES FOR A NEW COMPANY_METRIC_REPORTED 
    
    observeEvent(input$storenewcompany, {
      
      companyName3 <- input$companyNameText3
      ticker3 <- input$companyTickerText3
      companyNote3 <- input$companyNoteText3 
      metricName3 <- input$metricNameText3
      metricNote3 <- input$denoText3
      startQuarterLabel3 <- input$startingQuarterLabelText3
      startQuarterLastday3 <- input$startingQuarterText3
      #startQuarterLastday3 <- as.Date(startQuarterLastday3, format = "%m-%d-%Y")
      currentMonth3 <- as.Date(cut(Sys.Date(), "month"))
      metricdf3 <- data.frame(input$data3)
      metricdf3[,3]<- as.numeric(as.character(metricdf3[,3]))
      
      startQuarter3 <- startQuarterLastday3 - 5 
      month(startQuarter3) <- month(as.Date(startQuarter3)) - 2
      startQuarter3 <- as.Date(cut(startQuarter3,"month"))
      
      quarterStartDateSeries3 <- seq(startQuarter3,currentMonth3,by = "3 months")
      metricdf3$Start <- as.character(quarterStartDateSeries3)
      metricdf3 <- metricdf3 %>% filter(!is.na(X3))
      colnames(metricdf3) <- c("End","QuarterLabel",metricName3,"Start")
      
      output$table.output3 <- renderTable({ metricdf3[,c(2,4,1,3)]}
                                         , include.rownames = FALSE
                                         , include.colnames = TRUE
      ) 
      
      output$newcompanydetails <- renderUI({
        str1 <- paste0( "Company Name: ",companyName3) 
        str2 <- paste0("Company Ticker: ",ticker3)
        str3 <- paste0("Company Metric: ",metricName3)
        HTML(paste(str1, str2,str3, sep = '<br/>'))
      })
      
      output$matrix.output <- renderUI({
        fluidRow(
          h5("Information Entered:")
          ,htmlOutput("newcompanydetails")
          ,br()
          ,tableOutput('table.output3')
          ,h5("The above table will go into the database. Change anything in the table beside or Click the update button to store it")
          ,column(12, align = "center", actionButton("updatetable",'Update Database Table'))
        )
      })
      
      companyreactive$name <- companyName3
      companyreactive$ticker <- ticker3
      companyreactive$note <- companyNote3
      companyreactive$metric <- metricName3
      companyreactive$metricdeno <- metricNote3
      companyreactive$metrictable <- metricdf3
   
    }) 
    
    #### STORE THE NEW COMPANY VALUES INTO THE DATABASE 
    
    observeEvent(input$updatetable, {
      
      companystr <- "Company Database Error"
      
      tryCatch( {
        c_id <- get_comapny_id(con,"",companyreactive$ticker)
        if (c_id=="No Data"){
          newcompany <- data.frame(companyreactive$ticker, companyreactive$name,companyreactive$note)
          colnames(newcompany) <- c("Ticker","Name","Notes")
          companystr <- write_database(newcompany,"Company",con)
        }else{companystr <- "Company Already Exists"}

      },error = function(err){print(paste0("Company Already Exists",err))})
      
      metricstr <- "Metric Database error"
      tryCatch({ 
        
        companyid<-get_comapny_id(con,"",companyreactive$ticker)
        
        dbmetricslist = get_metric_list(con,companyid)
        
        if (class(dbmetricslist)=="data.frame"){
          if (!(companyreactive$metric %in% dbmetricslist$Metric)){
            newmetric <- data.frame(companyid,companyreactive$metricdeno,companyreactive$metric)
            colnames(newmetric) <- c("Company_id","Metric_unit","Metric")
            metricstr <- write_database(newmetric,"Metric",con)
          }
          else{metricstr <- "Metric Name already in database for this company"}
        }else{
          newmetric <- data.frame(companyid,companyreactive$metricdeno,companyreactive$metric)
          colnames(newmetric) <- c("Company_id","Metric_unit","Metric")
          metricstr <- write_database(newmetric,"Metric",con)
          
        }
        
        
      },error = function(err){print(paste0("Metric Name already in database for this company: ",err))})
      
      valuestr <- "Values Database Error"
      tryCatch({
        
        dbmetricslist = get_metric_list(con,companyid)
        metricid <- dbmetricslist[dbmetricslist$Metric==companyreactive$metric,]$id
        
        df <- companyreactive$metrictable
        #df$End <- create_quarter_end(df$Start)
        df$Company_id <- companyid
        df$Metric_id <- metricid
        
        toupload <- df[,c(5,6,2,4,1,3)]
        colnames(toupload) <- c("Company_id","Metric_id","Quarter","Start_date","End_date","Reported_value")
        toupload <- toupload %>% filter(!is.na(Reported_value))
        valuestr <- write_database(toupload,"Reported",con)
 
      },error = function(err){print(paste0("Database Error",err))})

      output$matrix.input <- renderUI({})

      output$matrix.output <- renderUI({fluidRow(
        
        h5("Database updated with the values")
        ,h5(companystr)
        ,h5(metricstr)
        ,h5(valuestr)
        
      )})

      existingcompanies <- sort(get_all_company_list(con)$Name)

      updateSelectInput(session,"existingcompanylist",choices = existingcompanies,selected = companyreactive$name)
      
      updateSelectInput(session,"downloadcomapnyname",choices = existingcompanies)
      
      updateSelectInput(session,"EarningsComapnyName",choices = existingcompanies)
      
      updateSelectInput(session,'ConsensusComapnyName',choices = existingcompanies,selected = "8X8")
      
      
    },label = "finalupdate3",autoDestroy=TRUE) 
 
    ###################################################################
    #### FOR A NEW METRIC IN A EXISTING COMPANY #### 
    ###################################################################
    
    quartervalues2 <- reactiveValues()
    
    observeEvent(input$submit2,{
      
      companyName2 <- input$existingcompanylist
      metricName2 <- input$metricNameText2
      metricNote2 <- input$denoText2
      startQuarterLabel2 <- input$startingQuarterLabelText2
      startQuarterLastday2 <- input$startingQuarterText2
      currentMonth2 <- as.Date(cut(Sys.Date(), "month"))
      startQuarter2 <- startQuarterLastday2 - 5
      month(startQuarter2) <- month(as.Date(startQuarter2)) - 2
      startQuarter2 <- as.Date(cut(startQuarter2,"month"))
      
      if(str_detect(startQuarterLabel2,"^Q[1-4]{1}-Y[0-9]{4}") & startQuarter2<currentMonth2 & metricName2!=""){
        
        quarterStartDateSeries2 <- seq(startQuarter2,currentMonth2,by = "3 months")
        
        if(length(quarterStartDateSeries2)==1){
          qtostart2 <- startQuarterLabel2
        }else{
          qtostart2 <- create_quarterlabel_list(startQuarterLabel2,length(quarterStartDateSeries2))
        }
        
        quarterEndDateSeries2 <- create_quarter_end(quarterStartDateSeries2)
        df2 <- create_empty_matrixdf(quarterEndDateSeries2,qtostart2,metricName2)

        quartervalues2$quarterStartDateSeries <- quarterStartDateSeries2
        quartervalues2$quarterEndDateSeries <- quarterEndDateSeries2
        quartervalues2$qtostart <- qtostart2
        
        output$matrix.input <- renderUI(fluidRow(
          h5(paste0("Input Table for "))
          ,matrixInput(inputId = 'data2', value = as.matrix(df2),class = "character")
          ,column(12, align = "center", actionButton("storenewmetric",'Store Table'))
        ))
 
        output$matrix.output <- renderUI(
          h5("Waiting for you to fill the table")
        )
        
       }else{
        output$matrix.input <- renderUI(
          h5("Check Metric Name, Quarter Label, Start Date")
        )
        output$matrix.output <- renderUI(
          h5("Waiting for you to fill the table")
        )
      }
      
    })
    
    reportedvalues2 <- reactiveValues()
    
    #### FINALISE THE REPORTED VALUES FOR AN NEW METRIC_REPORTED 
    
    observeEvent(input$storenewmetric, {
      
      companyName2 <- input$existingcompanylist
      metricName2 <- input$metricNameText2
      metricNote2 <- input$denoText2
      metricdf2 <- data.frame(input$data2)
      metricdf2[,3] <- as.numeric(as.character(metricdf2[,3]))
      quarterStartDateSeries2 <- quartervalues2$quarterStartDateSeries
      metricdf2$Start <- as.character(quarterStartDateSeries2)
      metricdf2 <- metricdf2 %>% filter(!is.na(X3))
      colnames(metricdf2) <- c("End","QuarterLabel",metricName2,"Start")
      
      
      output$table.output2 <- renderTable({ metricdf2[,c(2,4,1,3)]}
                                          , include.rownames = FALSE
                                          , include.colnames = TRUE
      ) 
      
      output$exisitngcompanydetails <- renderUI({
        str1 <- paste0( "Company Name: ",companyName2) 
        str2 <- paste0("Company Metric: ",metricName2)
        HTML(paste(str1, str2, sep = '<br/>'))
      })
      
      output$matrix.output <- renderUI({
        fluidRow(
          h5("Information Entered:")
          ,htmlOutput("exisitngcompanydetails")
          ,br()
          ,tableOutput('table.output2')
          ,h5("The above table will go into the database. Change anything in the table beside or Click the update button to store it")
          ,column(12, align = "center", actionButton("updatetable2",'Update Database Table'))
        )
      })
      
      reportedvalues2$name <- companyName2
      reportedvalues2$metric <- metricName2
      reportedvalues2$metrictable <- metricdf2
      reportedvalues2$deno <- metricNote2
    })
    
    #### STORE THE NEW METIRCS VALUES INTO THE DATABASE 
    
    observeEvent(input$updatetable2, {
      
      metricstr <- "Metric Database Error"
      tryCatch({ 
        
        companyid<-get_comapny_id(con,reportedvalues2$name,"")
        
        dbmetricslist = get_metric_list(con,companyid)
        
        if(class(dbmetricslist)=="data.frame"){
          if(!(reportedvalues2$metric %in% dbmetricslist$Metric)){
            newmetric <- data.frame(companyid,reportedvalues2$deno,reportedvalues2$metric)
            colnames(newmetric) <- c("Company_id","Metric_unit","Metric")
            metricstr <- write_database(newmetric,"Metric",con)
            
          }else{metricstr <- "Metric Name already in database for this company"}
        }else{
          newmetric <- data.frame(companyid,reportedvalues2$deno,reportedvalues2$metric)
          colnames(newmetric) <- c("Company_id","Metric_unit","Metric")
          metricstr <- write_database(newmetric,"Metric",con)
        }
      },error = function(err){print(paste0("Metric Name already in database for this company",err) )  })
      
      valuestr <- "Values Database Error"
      tryCatch({
        
        dbmetricslist = get_metric_list(con,companyid)
        metricid <- dbmetricslist[dbmetricslist$Metric==reportedvalues2$metric,]$id[1]
        
        df <- reportedvalues2$metrictable
        #df$End <- create_quarter_end(df$Start)
        df$Company_id <- companyid
        df$Metric_id <- metricid
 
        toupload <- df[,c(5,6,2,4,1,3)]
        colnames(toupload) <- c("Company_id","Metric_id","Quarter","Start_date","End_date","Reported_value")
        toupload <- toupload %>% filter(!is.na(Reported_value))
        valuestr <- write_database(toupload,"Reported",con)
        
      },error = function(err){print(paste0("Values Already Exists",err) )  })
      
 
      output$matrix.input <- renderUI({})
      output$matrix.output <- renderUI({
        fluidRow(
          h5("Database updated with the values")
          ,h5(metricstr)
          ,h5(valuestr)
          
        )
        })
      
      companyName <- input$existingcompanylist
      
      #### Updating the metric list for current session
      dbmetricslist <- get_metric_list(con,companyid)
      
      if(class(dbmetricslist)=="data.frame"){
        metricslist <- dbmetricslist$Metric
        updateSelectInput(session,"listofmetrics",
                          choices = metricslist,selected = reportedvalues2$metric)
      }else{
        updateSelectInput(session,"listofmetrics",
                          choices = "nometrics")
      }
    },label = "finalupdate2",autoDestroy=TRUE)
    
    ###################################################################
    #### DOWNLOADING COMPANY DATA ####
    ##################################################################
    
    observe({
      companyname <- input$downloadcomapnyname
      tryCatch({
        
        #con  <- dbConnect(RMySQL::MySQL(), host = host,dbname = dbname, user=user,password=password)
        companyid <- get_comapny_id(con,companyname,"")
        companyinfo <- get_comapny_info1(con,companyid)
        dbmetricslist <- get_metric_list(con,companyid)
        dbreportedvalues <- get_all_reported_metrics(con,companyid)
        
        if(class(dbreportedvalues)=="data.frame"){
          
          dbreportedvalues$Start_date <- as.character(date(dbreportedvalues$Start_date))
          dbreportedvalues$End_date <- as.character(date(dbreportedvalues$End_date))
          dbreportedvalues <- left_join(dbreportedvalues[,c(2,3,4,5,6)],dbmetricslist[,c("Metric","id")],by = c("Metric_id"="id"))
          dbreportedvalues <- dbreportedvalues %>% select(-Metric_id)
          dbreportedvalues <- spread(dbreportedvalues,Metric,Reported_value)
          dbreportedvalues <- dbreportedvalues[order(dbreportedvalues$Start_date),]
          output$company.info <- renderText(paste0("  Company Ticker: ",companyinfo$Ticker," || Company Description: ", companyinfo$Notes))
          output$table.download <- renderTable({dbreportedvalues},rownames = F,colnames = T)
          
          output$downloadtable <- downloadHandler(
            filename = function(){
              paste0(companyname, " Reported Data.csv",sep = "" )
            },
            content = function(file){
              write.csv(dbreportedvalues,file,row.names = FALSE)
            },
            contentType = "text/csv"
          )
          
        }else{output$table.download <- renderTable({data.frame("No data in the database")},rownames = F,colnames = F)
          
        }
        
        
        #lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
    
      },error = function(err){ print(paste0("No Values in the Database: ",err) )})
      
      
    })

    ####################################################################
    #### EARNINGS DATE STORAGE ####
    ####################################################################
    earningsreactivevalues <- reactiveValues()
    
    observe({
      companyid<-get_comapny_id(con,input$EarningsComapnyName,"")
      output$earnings <- renderTable(get_earnings_company(con,companyid))
      
    })
    
    observeEvent(input$submit4,{

      earningsquarterlabel <- input$EarningsQuarterLabel
      
      if(str_detect(earningsquarterlabel,"^Q[1-4]{1}-Y[0-9]{4}")){
        
        output$earningcompanydetails <- renderUI({
          str1 <- paste0( "Company Name: ",input$EarningsComapnyName) 
          str2 <- paste0("Said Quarter: ",input$EarningsQuarterLabel)
          str3 <- paste0("Earings Date: ",input$EarningsDate)
          HTML(paste(str1, str2,str3, sep = '<br/>'))
        })
        
        output$earnings.output <- renderUI({
          fluidRow(
            h5("Information Entered:")
            ,htmlOutput("earningcompanydetails")
            ,h5("The above table will go into the database. Change anything in the table beside or Click the update button to store it")
            ,column(12, align = "center", actionButton("updatetable4",'Update Database Table'))
          )
        })
        
        earningsreactivevalues$name <- input$EarningsComapnyName
        earningsreactivevalues$quarter <- input$EarningsQuarterLabel
        earningsreactivevalues$date <- input$EarningsDate
        
      }else{
        output$earnings.output <- renderUI(
          h5("Check Quarter Label")
        )
      }

      
    })
    
    #### STORE THE EARNINGS VALUES INTO THE DATABASE 
    
    observeEvent(input$updatetable4,{
      earningstr <- "Error in earnigns database"
      
      tryCatch({
        companyid<-get_comapny_id(con,earningsreactivevalues$name,"")
        earningdate <- earningsreactivevalues$date
        newearning <- data.frame(companyid,earningsreactivevalues$quarter,earningdate)
        colnames(newearning) <- c("Company_id","Quarter","Earning_Date")
        earningstr <- write_database(newearning,"Earningdates",con)
        
        output$earnings <- renderTable(get_earnings_company(con,companyid))
        
      },error = function(err){print(paste0("Earnigs Database Error",err) )  })
      
      output$earnings.output <- renderUI({
        fluidRow(
          h5("Database updated with the values")
          ,h5(earningstr)
          
        )
      })
    })
    
    ####################################################################
    #### CONSENSUS DATA STORAGE ####
    ####################################################################
    
    observe({
      companyName <- input$ConsensusComapnyName
      companyid <- get_comapny_id(con,companyName,"")
      dbmetricslist <- get_metric_list(con,companyid)
      
      existingconsensus <- get_consensus_company(con,companyid)
      existingconsensus$Consensus = existingconsensus$Consensus*100
      output$consensus <- renderTable(existingconsensus)
      
      if(class(dbmetricslist)=="data.frame"){
        metricslist <- dbmetricslist$Metric
        updateSelectInput(session,"consensusmetrics", choices = metricslist)
      }else{ updateSelectInput(session,"consensusmetrics",choices = "nometrics")
          }

      
    })
    
    consensusreactivevalues <- reactiveValues()
    
    observeEvent(input$submit5,{
      
      consensusquarterlabel <- input$ConsensusQuarterLabel
      if(str_detect(consensusquarterlabel,"^Q[1-4]{1}-Y[0-9]{4}")){
        
        output$consensuscompanydetails <- renderUI({
          str1 <- paste0( "Company Name: ",input$ConsensusComapnyName) 
          str2 <- paste0("Company Metric: ",input$consensusmetrics)
          str3 <- paste0("Said Quarter: ",input$ConsensusQuarterLabel)
          str4 <- paste0("Consensus Number: ",input$Consensus)
          HTML(paste(str1, str2,str3,str4, sep = '<br/>'))
        })
        
        output$consensus.output <- renderUI({
          fluidRow(
            h5("Information Entered:")
            ,htmlOutput("consensuscompanydetails")
            ,h5("The above table will go into the database. Change anything in the table beside or Click the update button to store it")
            ,column(12, align = "center", actionButton("updatetable5",'Update Database Table'))
          )
        })
        
        consensusreactivevalues$name <- input$ConsensusComapnyName
        consensusreactivevalues$metric <- input$consensusmetrics
        consensusreactivevalues$quarter <- input$ConsensusQuarterLabel
        consensusreactivevalues$consensus <- input$Consensus

        
      }else{
        output$consensus.output <- renderUI(
          h5("Check Quarter Label")
        )
        
      }
      
    })
    
    observeEvent(input$updatetable5,{
      consensusstr <- "Error in consensus database"
      
      tryCatch({
        
        companyid<-get_comapny_id(con,input$ConsensusComapnyName,"")
        dbmetricslist = get_metric_list(con,companyid)
        metricid <- dbmetricslist[dbmetricslist$Metric==input$consensusmetrics,]$id[1]
        
        newconsensus <- data.frame(companyid,metricid,input$ConsensusQuarterLabel,input$Consensus/100)
        colnames(newconsensus) <- c("Company_id","Metric_id","Quarter","Consensus")
        consensusstr <- write_database(newconsensus,"Consensus",con)
        
        existingconsensus <- get_consensus_company(con,companyid)
        existingconsensus$Consensus = existingconsensus$Consensus*100
        output$consensus <- renderTable(existingconsensus)
        
      },error = function(err){print(paste0("Consensus Database Error",err) )  })
      
      output$consensus.output <- renderUI({
        fluidRow(
          h5("Database updated with the values")
          ,h5(consensusstr)
          
        )
      })
    })
    
    
    #########################################################
    
  }
)


