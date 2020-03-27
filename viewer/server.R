################################### FUNDAMENTAL MODESL VIEWING ###################################

shinyServer(function(input, output,session) {
  
  ## TRAFFIC MODELS SUMMARY FILE
  observeEvent(input$submit1,{
    
    summary_category <- input$summary_category
    
    query <- paste0("select Name as 'Company Name',Ticker,Metric,ms.model_config_id,for_client as 'In Client File',
                    current_end_date as 'Current Date',current_quarter_label as 'Curent Quarter', round(rsq*100,2) as 'R-Squared',
                    data_points + 4 as 'Quarters in Model',round(yoy_estimate * 100,2) as 'Estimated YOY'
                    from fundamental_models.model_specification ms
                    join fundamental_models.model_current_quarter mcq on ms.model_config_id = mcq.model_config_id
                    join fundamental_models.model_description md on ms.model_config_id = md.model_config_id and mcq.current_end_date = md.end_date
                    left join fundamental_models.model_error_rate mer on ms.model_config_id = mer.model_config_id and mcq.current_end_date = mer.end_date
                    join fundamentals.Company C on ms.company_id = C.id
                    join fundamentals.Metric M on ms.metric_id = M.id
                    where forward_tracked = 1 and category = '", summary_category, "' order by Name;")
    
    summary_table <- dbGetQuery(tillotechdb_cons,query);
    if(nrow(summary_table)>0){
      output$summary_table <- renderDataTable(summary_table)
    }else{
      output$summary_table <- renderDataTable(data.frame("Note" = "No Models for this category"))
    }
    
    output$summary_file_download <- downloadHandler(
      filename = function(){
        paste0(summary_category, " - Live Models Summary.csv")
      },
      content = function(file){
        write.csv(summary_table,file,row.names = FALSE)
        
      },
      contentType = "text/csv"
    )
  })
  
  ## TICKER MODELS SESSION UPDATE
  observe({
    tn <- input$select_ticker
    ticker <- model_tickers$Ticker[model_tickers$Ticker_Name == tn]
    query <- paste0('select model_config_id from fundamental_models.model_specification ms
                    join fundamentals.Company C on ms.company_id = C.id
                    where Ticker = "',ticker,'";')
    model_ids <- dbGetQuery(tillotechdb_cons,query);
    updateSelectInput(session,"select_model_config_id",choices = unique(model_ids$model_config_id))
    
    model_tickers <<- get_model_tickers(tillotechdb_cons)
    model_tickers$Ticker_Name <<- paste0(model_tickers$Ticker," : ",model_tickers$category)
  })
  
  ## TRAFFIC MODEL GRAPHS
  observeEvent(input$submit2,{
    model_config_id <- input$select_model_config_id
    query <- paste0('select Ticker,Name,Metric,for_client,forward_tracked,category,current_end_date,current_quarter_label,not_forward_tracked_reason 
                    from fundamental_models.model_specification ms
                    join fundamental_models.model_current_quarter mcq on ms.model_config_id = mcq.model_config_id
                    join fundamentals.Company C on ms.company_id = C.id
                    join fundamentals.Metric M on ms.metric_id = M.id
                    where ms.model_config_id = "',model_config_id,'";')
    model_specifications <- dbGetQuery(tillotechdb_cons,query)
    if(nrow(model_specifications)==1){
      
      query <- paste0('select domain,mdl.domain_id,keyword,yoy_transformation,level_transformation,platform,traffic_type 
                     from fundamental_models.model_domain_list mdl
                      join strat_new.domains d on mdl.domain_id = d.domain_id
                      where model_config_id = "',model_config_id,'";')
      model_domain_list <- dbGetQuery(tillotechdb_cons,query)
      query <- paste0('select md.quarter_label,md.end_date,data_points,rsq,intercept,gradient,model_output,yoy_estimate,level_estimate,yoy_reported,level_reported,forecast_error,client_historical 
                      from fundamental_models.model_description md
                      left join fundamental_models.model_error_rate mer on md.model_config_id = mer.model_config_id and md.end_date = mer.end_date
                      where md.model_config_id = "',model_config_id, '";')
      model_error_rate <- dbGetQuery(tillotechdb_cons,query)
      query <- paste0('select end_date,quarter_label,tomahawk_metric,company_metric,tomahawk_yoy,company_yoy 
                      from fundamental_models.model_raw_data where model_config_id = "',model_config_id,'" order by end_date;')
      model_raw_data <- dbGetQuery(tillotechdb_cons,query)
      
      ## DATA TABLES ----------------------------
      output$model_specification <- renderTable(model_specifications)
      output$model_domain_list  <- renderTable(model_domain_list)
      output$model_error_rate <- renderTable(model_error_rate)
      
      ## PLOTS --------------------------------
      output$regxylevel <- renderPlot(ggplotRegression(lm(company_metric ~ tomahawk_metric, data = model_raw_data)))
      output$regxyyoy <- renderPlot(ggplotRegression(lm(company_yoy ~ tomahawk_yoy, data = vars)))
      vars <- model_raw_data %>% select(end_date,tomahawk_yoy,company_yoy) %>% filter(!is.na(tomahawk_yoy))
      scale = max(vars$company_yoy, na.rm = T)/max(vars$tomahawk_yoy, na.rm = T)
      py <- ggplot(vars, aes(x = end_date)) +
        geom_line(aes(y = tomahawk_yoy, colour = "YoY Traffic",group = 1),size = 1) +
        geom_line(aes(y = company_yoy, colour = "YoY Metric",group = 2 ),size = 1) +
        geom_point(aes(y = tomahawk_yoy, colour = "YoY Traffic"),size = 2) +
        geom_point(aes(y = company_yoy, colour = "YoY Metric"),size = 2) +
        scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "YoY Metric")) + 
        theme(legend.position="bottom") + labs(y = "YoY Traffic",x = "Date",colour = "Variables")
      output$yoyxy <- renderPlot(py)
      
      scale = max(model_raw_data$company_metric, na.rm = T)/max(model_raw_data$tomahawk_metric, na.rm = T)
      pl <- ggplot(model_raw_data,aes(x = end_date)) + 
        geom_line(aes(y = tomahawk_metric,colour = "Traffic",group = 1),size = 1) + 
        geom_line(aes(y = company_metric/scale,colour = "Metric",group = 2),size = 1) + 
        geom_point(aes(y = tomahawk_metric,colour = "Traffic"),size = 2) +
        geom_point(aes(y = company_metric/scale,colour = "Metric"),size = 2) + 
        scale_y_continuous(sec.axis = sec_axis(~.*scale,name = "Metric")) + 
        theme(legend.position = "bottom") + labs(colour = "Variables")
      output$levelxy <- renderPlot(pl)

      output$model_raw_data <- renderTable(model_raw_data)
    }else{
      
      output$model_specification <- renderDataTable(data.frame("Note" = "No model data for this model_config_id"))
      output$model_domain_list  <- renderDataTable(data.frame("Note" = "No model data for this model_config_id"))
      output$model_error_rate <- renderDataTable(data.frame("Note" = "No model data for this model_config_id"))
      output$yoyxy <- renderPlot(ggplot())
      output$levelxy <- renderPlot(ggplot())
      output$regxylevel <- renderPlot(ggplot())
      output$regxyyoy <- renderPlot(ggplot())
    }
    
    output$raw_data_download <- downloadHandler(
      filename = function(){
        paste0(model_specifications$Ticker," - ",model_config_id, " - Model Raw Data.csv")
      },
      content = function(file){
        write.csv(model_raw_data,file,row.names = FALSE)
        
      },
      contentType = "text/csv"
    )
    
    
  })
  
  ## TRAFFIC MODEL QUARTER ESTIMATEs
  observeEvent(input$submit3,{
    quarter_date <- input$select_date_error
    query <- paste0("select Name as Company,Ticker,Metric,category as Category,mer.model_config_id as Model,for_client as `Live - Client`,forward_tracked as `Live`,
                    not_forward_tracked_reason as `DQed Reason`,client_historical as `Client Presented`,
                    quarter_label as Quarter,end_date as `Quarter Date`,round(yoy_estimate*100,2) as `YOY Estimate`,round(Consensus*100,2) as `YOY Consensus`,
                    round(yoy_reported*100,2) as `YOY Reported`,round(level_estimate,2) as `Metric Estimate`,round(level_reported,2) as `Metric Reported`,round(forecast_error*100,2) as `Forecast Error`,Earning_Date as `Report Date`
                    from fundamental_models.model_error_rate mer
                    join fundamental_models.model_specification ms on mer.model_config_id = ms.model_config_id
                    join fundamentals.Company C on C.id = ms.company_id
                    left join fundamentals.Metric M on M.id = ms.metric_id
                    left join fundamentals.Earningdates E on E.Quarter = mer.quarter_label and E.Company_id = ms.company_id
                    left join fundamentals.Consensus CN on CN.Quarter = mer.quarter_label and CN.Company_id = ms.company_id and CN.Metric_id = ms.metric_id
                    where end_date = '",quarter_date,"';")
    error_table <- dbGetQuery(tillotechdb_cons,query)
    
        
    if(nrow(error_table)>0){
      
      error_summary <- error_table %>% group_by(Category) %>% summarise(`Error for all estimates` = mean(abs(`Forecast Error`),na.rm = T)) %>% 
        left_join(error_table %>% group_by(Category) %>% filter(`Client Presented` == 1) %>% 
                    summarise(`Error for Client Presented estimates` = mean(abs(`Forecast Error`),na.rm = T)),by = 'Category') %>%
        left_join(error_table %>% group_by(Category) %>% filter(`Live - Client` == 1) %>% 
                    summarise(`Error for Live Model estimates` = mean(abs(`Forecast Error`),na.rm = T)),by = 'Category')
      
      output$error_summary <- renderTable(error_summary)
      output$error_table <- renderDataTable(error_table)
    }else{
      output$error_summary <- renderTable(data.frame("Note" = "No summary"))
      output$error_table <- renderDataTable(data.frame("Note" = "No Models for this date"))
    }
    
  })
  
  observeEvent(input$submit4,{
    ticker <- model_tickers$Ticker[model_tickers$Ticker_Name == input$select_ticker_error]
    query <- paste0("select Name as Company,Ticker,Metric,category as Category,mer.model_config_id as Model,for_client as `Live - Client`,forward_tracked as `Live`,
                    not_forward_tracked_reason as `DQed Reason`,client_historical as `Client Presented`,
                    quarter_label as Quarter,end_date as `Quarter Date`,round(yoy_estimate*100,2) as `YOY Estimate`,round(Consensus*100,2) as `YOY Consensus`,
                    round(yoy_reported*100,2) as `YOY Reported`,round(level_estimate,2) as `Metric Estimate`,round(level_reported,2) as `Metric Reported`,round(forecast_error*100,2) as `Forecast Error`,Earning_Date as `Report Date`
                    from fundamental_models.model_error_rate mer
                    join fundamental_models.model_specification ms on mer.model_config_id = ms.model_config_id
                    join fundamentals.Company C on C.id = ms.company_id
                    left join fundamentals.Metric M on M.id = ms.metric_id
                    left join fundamentals.Earningdates E on E.Quarter = mer.quarter_label and E.Company_id = ms.company_id
                    left join fundamentals.Consensus CN on CN.Quarter = mer.quarter_label and CN.Company_id = ms.company_id and CN.Metric_id = ms.metric_id
                    where Ticker = '",ticker,"';")
    error_table <- dbGetQuery(tillotechdb_cons,query)
    
    
    if(nrow(error_table)>0){
      
      error_summary <- error_table %>% group_by(Metric) %>% summarise(`Error for all estimates` = mean(abs(`Forecast Error`),na.rm = T)) %>% 
      left_join(error_table %>% group_by(Metric) %>% filter(`Client Presented` == 1) %>% 
            summarise(`Error for Client Presented estimates` = mean(abs(`Forecast Error`),na.rm = T)),by = 'Metric') %>%
      left_join(error_table %>% group_by(Metric) %>% filter(`Live - Client` == 1) %>% 
            summarise(`Error for Live Model estimates` = mean(abs(`Forecast Error`),na.rm = T)),by = 'Metric')
      
      output$error_summary <- renderTable(error_summary)
      output$error_table <- renderDataTable(error_table)
    }else{
      output$error_summary <- renderTable(data.frame("Note" = "No summary"))
      output$error_table <- renderDataTable(data.frame("Note" = "No Models for this ticker"))
    }
    
  })
  
  ## TRAFFIC MODELS ERROR TRACKER
  observeEvent(input$submit5,{
    
    res <- get_tables_for_reportcard(tillotechdb_cons)
    error_rate <- res[[1]]
    model_specs <- res[[2]]
    category <- input$industry_category
    model_specs <- model_specs[model_specs$category %in% category,]
    error_rate <- error_rate[error_rate$model_config_id %in% model_specs$model_config_id,]
    error_rate$abs_forecast_error <- abs(error_rate$forecast_error)
    traffic_models_report_card <- data.frame()
    ### Model Counts
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "Models created","value" = nrow(model_specs)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Models currently being forward tracked","value" = nrow(model_specs[model_specs$forward_tracked==1,])))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Models currently being presented to clients","value" = nrow(model_specs[model_specs$for_client==1,])))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Models dead","value" = nrow(model_specs[model_specs$forward_tracked==0,])))
    
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "","value" = ""))
    
    ### Company Counts
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "Companies explored","value" = length(unique(model_specs$company_id))))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Companies cuurently being forward tracked","value" = length(unique(model_specs$company_id[model_specs$forward_tracked==1]))))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Companies cuurently being presented to clients","value" = length(unique(model_specs$company_id[model_specs$for_client==1]))))
    
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "","value" = ""))
    
    ### All Models error and rsq
    rsq_mmm <- paste0(round(mean(error_rate$rsq[!is.na(error_rate$forecast_error)]),2)," / ",
                      round(min(error_rate$rsq[!is.na(error_rate$forecast_error)]),2)," / ",round(max(error_rate$rsq[!is.na(error_rate$forecast_error)]),2))
    
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "Total Forecasts Created","value" = sum(!is.na(error_rate$forecast_error))))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Mean Absolute Forecast Error","value" = round(mean(error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Max Absolute Forecast Error","value" = round(max(error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Min Absolute Forecast Error","value" = round(min(error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Mean/Min/Max RSQ","value" = rsq_mmm))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "","value" = ""))
    
    ### Client Models error and rsq
    live_error_rate <- error_rate[error_rate$client_historical == 1,]
    rsq_mmm <- paste0(round(mean(live_error_rate$rsq[!is.na(live_error_rate$forecast_error)]),2)," / ",
                      round(min(live_error_rate$rsq[!is.na(live_error_rate$forecast_error)]),2)," / ",round(max(live_error_rate$rsq[!is.na(live_error_rate$forecast_error)]),2))
    
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "Total Forecasts Presented to Clients","value" = sum(!is.na(live_error_rate$forecast_error))))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Mean Absolute Forecast Error","value" = round(mean(live_error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Max Absolute Forecast Error","value" = round(max(live_error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Min Absolute Forecast Error","value" = round(min(live_error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Mean/Min/Max RSQ","value" = rsq_mmm))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "","value" = ""))
    
    ### Dead Models error and rsq
    dead_error_rate <- error_rate[error_rate$model_config_id %in% model_specs$model_config_id[model_specs$forward_tracked==0],]
    rsq_mmm <- paste0(round(mean(dead_error_rate$rsq[!is.na(dead_error_rate$forecast_error)]),2)," / ",
                      round(min(dead_error_rate$rsq[!is.na(dead_error_rate$forecast_error)]),2)," / ",round(max(dead_error_rate$rsq[!is.na(dead_error_rate$forecast_error)]),2))
    
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "Total Forecasts for Dead models","value" = sum(!is.na(dead_error_rate$forecast_error))))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Mean Absolute Forecast Error","value" = round(mean(dead_error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Max Absolute Forecast Error","value" = round(max(dead_error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Min Absolute Forecast Error","value" = round(min(dead_error_rate$abs_forecast_error,na.rm = T),3)))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "---Mean/Min/Max RSQ","value" = rsq_mmm))
    traffic_models_report_card <- rbind(traffic_models_report_card,data.frame("header" = "","value" = ""))
    output$report_card <- renderDataTable(traffic_models_report_card)
    
    output$models_report_card_download <- downloadHandler(
      filename = function(){
        "Models_Report_Card.csv"
      },
      content = function(file){
        write.csv(traffic_models_report_card,file,row.names = FALSE)
        
      },
      contentType = "text/csv"
    )
    
  })

  })