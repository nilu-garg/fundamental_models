
################################### FORWARD TRACKING FUNDAMENTAL MODELS ###################################

shinyServer(function(input, output,session) {
  
 ## UPDATES COMPANY TICKERS WITH NEW  MODELS   
 observe({
    companyids <- get_all_forward_tracked_tickers(fund_con,model_con)
    companyids <- companyids[order(companyids$Ticker),]
    updateSelectInput(session,"company_ticker",choices = unique(companyids$Ticker))
  })
  
 ## CHECKING FOR MISSING DATA   
 observeEvent(input$submit1,{
   model_specification_list <- get_all_forward_tracked_models(model_con)
   last_month_in_db <- as.Date(cut(Sys.Date(),"month"))
   month(last_month_in_db) <- month(last_month_in_db)-1
   
   missing_data = data.frame()
   withProgress( message = "Checking Database",value = 0,{
   for (m in 1:nrow(model_specification_list)){
     incProgress(1/nrow(model_specification_list),detail = paste("Woking on ",m))
     model_config_id <- model_specification_list$model_config_id[m]
     model_domain_list <- get_model_domain_list(model_con,model_config_id)
     for(d in 1:nrow(model_domain_list)){
       domain_id <- model_domain_list$domain_id[d]
       platform <- platform_client_db_versions(model_domain_list$platform[d])
       print(d)
       ispprequired <- !(is.na(model_domain_list$keyword[d]))
       res <- get_domain_id_dbdetails(traffic_con,domain_id)
       pp_info <- res[[2]]
       domain_info <- res[[1]]
       domain <- unique(domain_info$domain)
       if(domain_info$`max(date)`[domain_info$platform == platform] != last_month_in_db){
         missing_data <- rbind(missing_data,data.frame("domain" = domain,"value" = "Traffic data not upto date"))
       }
       if(nrow(pp_info)>0 & ispprequired==TRUE){
         if(pp_info$`max(date)`[pp_info$platform == platform]!= last_month_in_db){
           missing_data <- rbind(missing_data,data.frame("domain" = domain,"value" = "PP data not upto date"))
         }
       }
       
     }
   }
   }) 
   if(nrow(missing_data)>0){
     output$missing_data <- renderDataTable(missing_data)
   }else{
     output$missing_data <- renderDataTable(data.frame("Note" = "Database upto date for forwardtracking"))
   }
   
 })

 ## FORWARD TRACKING MODELS FOR A NEW QUARTER   
 observeEvent(input$submit2,{
    
    ced <- as.character(input$current_ending_quarter)
    models_toupdate <- get_models_toestimate(model_con,ced)
    excel_updates <- data.frame()
    output$total_model_toupdate <- renderText(paste0("Total models updating this quarter: ",nrow(models_toupdate)))
    
    if (nrow(models_toupdate)<1 || ced >= as.Date(cut(Sys.Date(), "month"))){
      output$client_file_estimate <- renderDataTable(data.frame("Error Message" = c("Enter the end date of the completed current quarter")))
    }else{
      withProgress( message = "Calculating Estimates",value = 0,{
        
        for(mcid in 1:nrow(models_toupdate)){
          ### Getting individual model specifications
          incProgress(1/nrow(models_toupdate),detail = paste("Working on ",mcid))
          
          model_config_id <- models_toupdate$model_config_id[mcid]
          company_id <- models_toupdate$company_id[mcid]
          metric_id <- models_toupdate$metric_id[mcid]
          mdinfo <- get_all_info_for_model(model_con,model_config_id)
          ms <- mdinfo[[1]]
          md <- mdinfo[[2]]
          mcq <- mdinfo[[3]]
          mrd <- mdinfo[[4]]
          mer <- mdinfo[[5]]
          mdl <- mdinfo[[6]]
          
          
          if(nrow(mer)<1 || identical(is.na(mer$level_estimate[mer$end_date==ced]),logical(0))){
            
            companyinfo <- get_comapny_info1(fund_con,company_id)
            print(paste0("Company Working on: ",companyinfo$Ticker," with model_config_id: ",model_config_id))
            
            ### Getting Information from fundamentals database
            dbreportedvalues <- get_reported_metrics(fund_con,company_id,metric_id)
            dbreportedvalues$Start_date <- as.Date(dbreportedvalues$Start_date)
            quarterlist <- dbreportedvalues %>% select("Quarter","Start_date","End_date")
            
            ### Level and YOY Transformation and Scaling
            tom <- quarterlist %>% select("Start_date","Quarter")
            colnames(tom) <- c("date","quarterlabel")
            depvar <- dbreportedvalues[,c("Start_date", "Reported_value")]
            
            if(aggregate(mdl$keyword,list(mdl$domain_id),paste,collapse = "")$x == "NA"){
              indvar <- get_indvar_T(traffic_con,mdl,tom)
            }else{
              indvar <- get_indvar_K(traffic_con,mdl,tom)
            }
            
            leveltransformation <- level_transform_client_db_versions(unique(mdl$level_transformation))
            indvar = level_transform(indvar,leveltransformation)
            if (unique(mdl$level_transformation) == '4-QMA-B'){
              colnames(depvar) <- c('Start_date','page_activity')
              depvar <- level_transform(depvar,leveltransformation)
              colnames(depvar) <- c('Start_date','Reported_value')
            }
            yoytransformation <- yoy_transform_client_db_versions(unique(mdl$yoy_transformation))
            yoytransform_data <- yoy_transform(indvar,depvar,yoytransformation)
            indvar <- yoytransform_data[[1]]
            depvar <- yoytransform_data[[2]]
            
            ### Specifying the quarter and getting model output
            
            indvar$end_date <- create_quarter_end(indvar$Date) 
            indvar$Traffic <-  as.numeric(indvar$Traffic)
            indvar$YOY <- as.numeric(indvar$YOY)
            
            depvar$end_date <- create_quarter_end(depvar$Date)
            prev_ced <- as.Date(ced)
            year(prev_ced) = year(prev_ced)-1
            yed <- indvar %>% filter(end_date == md$end_date[md$end_date == mcq$current_end_date])
            cme_output <- cal_model_estimate(yed$YOY,md$intercept[md$end_date == mcq$current_end_date],md$gradient[md$end_date == mcq$current_end_date],
                                             yoytransformation,depvar$Metric[which(depvar$end_date == prev_ced)]) 
            

            updatedrow <- data.frame("model_config_id" = model_config_id,"start_date" = yed$Date,"end_date" = ced,
                                     "quarter_label" = md$quarter_label,
                                     "tomahawk_metric" = yed$Traffic,"company_metric" = NA,"tomahawk_yoy" = yed$YOY,"company_yoy" = NA)
            updatedrow$model_raw_data_id <- mrd$model_raw_data_id[mrd$end_date == ced]
            query <- paste0("DELETE FROM model_raw_data WHERE model_raw_data_id = ",mrd$model_raw_data_id[mrd$end_date == ced],";")
            dbExecute(model_con,query)
            write_database(updatedrow[,c(9,1,2,3,4,5,6,7,8)],"model_raw_data",model_con)
            if(models_toupdate$for_client[mcid]==1){
              client_historical = 1
            }else{
              client_historical = 0
            }
            mer <- data.frame("model_config_id" = model_config_id,"quarter_label" = md$quarter_label,"end_date" = ced,
                              "model_output" = cme_output[1],"yoy_estimate" = cme_output[2],"level_estimate" = cme_output[3],
                              "client_historical" = client_historical)
            write_database(mer,"model_error_rate",model_con)
            if(models_toupdate$for_client[mcid]==1){
              excel_updates <- rbind(excel_updates,data.frame("category" = ms$category,"ticker" = companyinfo$Ticker,"end_date" = ced,"quarter_label" = md$quarter_label,
                                                              "tomahawk_metric" = yed$Traffic,"company_metric" = NA,"tomahawk_yoy" = yed$YOY,"company_yoy" = NA))
            }

            }
          
          
        }
        
      })
      
      if(nrow(excel_updates)>0){
        output$client_file_estimate <- renderDataTable(excel_updates)
      }else{
        output$client_file_estimate <- renderDataTable(data.frame("Error Message" = c(paste0("Estiamtes for ",ced," are already in the database. Check the viewer app"))))
      }
      
      output$new_quarter_download <- downloadHandler(
        filename = function(){
          paste0(as.character(ced),"_estimates.csv")
        },
        content = function(file){
          write.csv(excel_updates, file = file ,sep = ",", row.names = F, col.names = T)
          
        },
        contentType = "text/csv"
      )
    }
  })

 ## GETTING MONTHLY DATA FOR MODELS  
 observeEvent(input$submit3,{
   
    monthlymodels <- get_all_client_tracked_models(model_con,fund_con)   
    monthlymodels <- monthlymodels[monthlymodels$category %in% input$mon_track_companyname,]
    
    withProgress( message = "Getting Monthyl Data",value = 0,{
      
   for (m in 1:nrow(monthlymodels)){
      model_config_id <- monthlymodels$model_config_id[m]
      mdl <- get_model_domain_list(model_con,model_config_id)
      ticker <- monthlymodels$Ticker[m]
      print(ticker)
      incProgress(1/nrow(monthlymodels),detail = paste("Woking on ",ticker))
      eng_m <- m_of_domain(traffic_con,mdl)
      setnames(eng_m,"page_activity",ticker)
      
      if(m==1){
        mon_tracker <- eng_m
      }else{
        mon_tracker <- full_join(mon_tracker,eng_m,by = "date")
      }

    }
    
    mon_tracker <- mon_tracker[order(mon_tracker$date),] 
    }) 
    
    mon_tracker <- mon_tracker %>% mutate_if(is.numeric, round, digits = 3)
    output$mon_tracker_df <- renderDataTable(mon_tracker)
    
    output$mon_tracker_download <- downloadHandler(
      filename = function(){
            "Monthly Tracker Data.csv"
      },
      content = function(file){
        write.csv(mon_tracker,file,row.names = FALSE)
        
      },
      contentType = "text/csv"
    )
    
    
  })
  
 ## FORWARD TRACKING MODEL FOR NEW REPORTED FUNDAMENTALS 
 observeEvent(input$submit4,{
   
   ticker <- input$company_ticker
   companyid <- companyids$id[companyids$Ticker == ticker]
   models_toupdate <- get_all_info_for_model(model_con,'',companyid)
   model_config_ids <- models_toupdate[[1]]$model_config_id
   print(model_config_ids)
   msa <- models_toupdate[[1]]
   mda <- models_toupdate[[2]]
   mcqa <- models_toupdate[[3]]
   mrda <- models_toupdate[[4]]
   mera <- models_toupdate[[5]]
   mdla <- models_toupdate[[6]]
   model_status = data.frame()
   
   for (model_config_id in model_config_ids){
        mrd <- mrda[mrda$model_config_id == model_config_id,]
        mdl <- mdla[mdla$model_config_id == model_config_id,]
        mer <- mera[mera$model_config_id == model_config_id,]
     if(is.na(mrd$company_metric[nrow(mrd)]) && mrd$end_date[nrow(mrd)] < as.Date(cut(Sys.Date(),"month"))){
       
       dbreportedvalues <- get_reported_metrics(fund_con,companyid,msa$metric_id[msa$model_config_id == model_config_id])
       dbreportedvalues$Start_date <- as.Date(dbreportedvalues$Start_date)
       quarterlist <- dbreportedvalues %>% select("Quarter","Start_date","End_date")
       
       current_datapoints <- get_model_datapoints(model_con,model_config_id)
       
       ### Level and YOY Transformation and Scaling
       tom <- quarterlist %>% select("Start_date","Quarter")
       colnames(tom) <- c("date","quarterlabel")
       depvar <- dbreportedvalues[,c("Start_date", "Reported_value")]
       
       if(aggregate(mdl$keyword,list(mdl$domain_id),paste,collapse = "")$x == "NA"){
         indvar <- get_indvar_T(traffic_con,mdl,tom)
       }else{
         indvar <- get_indvar_K(traffic_con,mdl,tom)
       }
       
       leveltransformation <- level_transform_client_db_versions(unique(mdl$level_transformation))
       indvar = level_transform(indvar,leveltransformation)
       if (unique(mdl$level_transformation) == '4-QMA-B'){
         colnames(depvar) <- c('Start_date','page_activity')
         depvar <- level_transform(depvar,leveltransformation)
         colnames(depvar) <- c('Start_date','Reported_value')
       }
       yoytransformation <- yoy_transform_client_db_versions(unique(mdl$yoy_transformation))
       yoytransform_data <- yoy_transform(indvar,depvar,yoytransformation)
       vars_output <- csv_vars_output(yoytransform_data[1][[1]],yoytransform_data[2][[1]])
       vars <- vars_output[3][[1]]
       csv_vars <- vars_output[1][[1]]
       
       res <- just_reg(vars[(nrow(vars)-current_datapoints):nrow(vars),],csv_vars,yoytransformation,'platform','form',leveltransformation,"Reported_value",'domainlist')
       csv_vars <- raw_data_standardizing(res[[3]],quarterlist)
       
       model_current_quarter <- data.frame(model_config_id = model_config_id, current_quarter_label = csv_vars$quarterlabel[nrow(csv_vars)],
                                           current_end_date = csv_vars$End_date[nrow(csv_vars)])
       
       ### MD
       model_description <- data.frame(model_config_id = model_config_id, quarter_label = csv_vars$quarterlabel[nrow(csv_vars)],
                                end_date = csv_vars$End_date[nrow(csv_vars)],
                                data_points = res[[1]]$numpts.yoy, rsq = res[[1]]$rsq.yoy,
                                intercept = res[[1]]$intcoeff.yoy, gradient = res[[1]]$xcoeff.yoy)
       
       
       if (model_description$rsq>=0.45){
         model_status_text <- "Can be forward tracked"
       }else{
         model_status_text <- "Review the model - might be broken"
       }
       print(model_status_text)
       
       ### MER
       model_error_rate <- get_model_error_rate(model_con,model_config_id)
       if(nrow(model_error_rate) == 1){
         if(yoytransformation == "yoydiff"){
           prequarlevel = csv_vars$Metric[(which(as.Date(csv_vars$End_date) == as.Date(model_error_rate$end_date))-4)]
           model_error_rate$yoy_reported <- csv_vars$YOY.dep[as.Date(csv_vars$End_date) == as.Date(model_error_rate$end_date)]/prequarlevel
         }else{
           model_error_rate$yoy_reported <- csv_vars$YOY.dep[as.Date(csv_vars$End_date) == as.Date(model_error_rate$end_date)]
         }
         
         model_error_rate$level_reported <- csv_vars$Metric[as.Date(csv_vars$End_date) == as.Date(model_error_rate$end_date)]  
         model_error_rate$forecast_error <- model_error_rate$level_estimate/model_error_rate$level_reported-1
       }
       
       ### MRD
       csv_vars['model_config_id'] = model_config_id
       colnames(csv_vars) <- c("start_date","tomahawk_metric","tomahawk_yoy","company_metric","company_yoy","quarter_label","end_date","model_config_id")
       raw_data <- csv_vars %>% select(model_config_id,start_date,end_date,quarter_label,tomahawk_metric,company_metric,tomahawk_yoy,company_yoy)
       lastrowindb <- mrd[is.na(mrd$company_metric),]
       if (lastrowindb$end_date == raw_data$end_date[nrow(raw_data)-1]){
         print("WORKS")
         query <- paste0("DELETE FROM fundamental_models.model_raw_data WHERE model_raw_data_id = ",lastrowindb$model_raw_data_id,";")
         dbExecute(model_con,query)
         updaterowindb <- raw_data[(raw_data$end_date == lastrowindb$end_date),]
         updaterowindb$model_raw_data_id <- lastrowindb$model_raw_data_id
         write_database(updaterowindb[,c(9,1,2,3,4,5,6,7,8)],"model_raw_data",model_con)
         write_database(raw_data[nrow(raw_data),],"model_raw_data",model_con)
         
         write_database(model_description,"model_description",model_con)
         
         mer_id <- mer$model_error_rate_id[mer$end_date == model_error_rate$end_date]
         query <- paste0("DELETE FROM fundamental_models.model_error_rate WHERE model_error_rate_id=",mer_id,";")
         dbExecute(model_con,query)
         model_error_rate$model_error_rate_id <- mer_id
         write_database(model_error_rate[,c(11,1,2,3,4,5,6,7,8,9,10)],"model_error_rate",model_con)
         
         query <- paste0("UPDATE fundamental_models.model_current_quarter SET current_end_date= '",model_current_quarter$current_end_date,
                         "', current_quarter_label = '",model_current_quarter$current_quarter_label,
                         "' WHERE model_config_id ='",model_current_quarter$model_config_id,"';")
         dbExecute(model_con,query)
       }
       
       model_status <- rbind(model_status,data.frame("ticker" = ticker,"model_config_id" = model_config_id,
                            "client_hitorical" = model_error_rate$client_historical,                         
                            "rsq" = model_description$rsq,"model_status" = model_status_text,
                            "estimated_value" = model_error_rate$yoy_estimate, "reported_value" = model_error_rate$yoy_reported,
                            "level_reported" = model_error_rate$level_reported,
                            "model_error_rate" = model_error_rate$level_estimate/model_error_rate$level_reported-1))
       
       
     }
   }
   
   if(nrow(model_status)>0){
     output$model_status <- renderTable(model_status)
   }else{
     output$model_status <- renderTable(data.frame("Error.Message" = paste0("Models for ",
                            ticker," are uptodate in the database,view the data in excel or tableau file")))
    }

 })
 
})    
    
