
################################### GENERATING MODELS ###################################

shinyServer(function(input, output,session) {
  
  ### For Tab1 ---------------------------------------------------------------------------------------------
  
  observeEvent(input$submit1,{
    
    domain <- input$companydomain
    domains_table <- get_domain_dbdetails(traffic_con,domain)
    traff_db <- domains_table[1][[1]]
    if(nrow(traff_db)>0){
      output$traff_db <- renderTable(traff_db)
    }else{
      output$traff_message <- renderTable(data.frame("Message" = paste0("No traffic data for ",domain," in the database")))
    }
    pp_db <- domains_table[2][[1]]
    if(nrow(pp_db)>0){
      output$pp_db <- renderTable(pp_db)
    }else{
      output$pp_message <- renderTable(data.frame("Message" = paste0("No pp data for ",domain," in the database")))
    }
    
  }) 
  
  ### For Tab2 ---------------------------------------------------------------------------------------------

  observeEvent(input$submit2,{
    
    domain <- input$exploredomain
    domain_id <- get_domain_id(traffic_con,domain)
    if (class(domain_id)=="integer"){
      
      pp_data <- get_pp_table(traffic_con,domain_id,'total')
      if (nrow(pp_data)>0){
        
        setDT(pp_data)[, "firstfolder" := tstrsplit(pg_url, "/")[1]]
        firstfoldercounts <- data.table(table(pp_data$firstfolder))
        names(firstfoldercounts) = c("firstfolder","frequency")
        firstfoldercounts <- firstfoldercounts[order(-frequency),]
        m <-  mean(firstfoldercounts$frequency)
        firstfoldercounts <- firstfoldercounts[firstfoldercounts$frequency>=m]
        output$subdomaindf <- renderDataTable(firstfoldercounts)
        setDT(pp_data)[,"keywords" := gsub("/",", ",gsub("[&%.?=-]","/",gsub(domain,"",tolower(pg_url))))]
        
        kf <- data.frame(words = unlist(strsplit(tolower(pp_data$keywords), ", "))) %>% group_by(words) %>% 
          dplyr::summarise(n = n()) %>% arrange(desc(n))
        setDT(kf)[,"Len" := nchar(as.character(words))]
        kf <- subset(kf,Len>1 & Len<25)
        output$keyworddf <- renderDataTable(kf)
        output$ppdf <- renderDataTable(pp_data[,c(1,2,3)])
      }else{
        output$subdomaindf <- renderDataTable(data.frame("Message" = "Popular pages data missing from strat table"))
        output$ppdf <- renderDataTable(data.frame())
        output$keyworddf <- renderDataTable(data.frame())
      }
      

    }else{
      output$subdomaindf <- renderDataTable(data.frame("Message" = "Enter correct domain"))
      output$ppdf <- renderDataTable(data.frame())
      output$keyworddf <- renderDataTable(data.frame())
      
    }
  }) 
  
  ### For Tab3 ---------------------------------------------------------------------------------------------
  
  results_reactive <- reactiveValues()
  
  observeEvent(input$submit3,{
    
    # GETTING INPUT PARAMETERS --------------------------------------------------------------------
    ticker <- input$companyticker
    domainlist <- input$domainlist
    
    # GETTING DATA FROM FUNDAMENTALS DATABASE --------------------------------------------------------------------
    
    companyinfo <- get_comapny_info(fund_con,ticker)
    dbmetricslist <- get_metric_list(fund_con,companyinfo$id)
    dbreportedvalues <- metric_long_to_wide(get_all_reported_metrics(fund_con,companyinfo$id),dbmetricslist)
    quarterlist <- dbreportedvalues %>% select("Quarter","Start_date","End_date")
    metricnames <- dbmetricslist$Metric[dbmetricslist$FT_flag==1]
    
    # CREATING DOMAIN COMBINATIONS --------------------------------------------------------------------
    
    domainlist <- unlist(strsplit(domainlist, ";"))
    maindomain <- domainlist[1]
    comb_list <- list()
    for (i in 1:length(domainlist)){
      combos <- combn(domainlist, i, simplify = FALSE)
      comb_list <- c(comb_list, combos)
    }
    combdf <- data.frame(combo = matrix(comb_list))
    
    # SETTING IDS FOR LOOPING --------------------------------------------------------------------
    
    tom <- quarterlist %>% select("Start_date","Quarter")
    colnames(tom) <- c("date","quarterlabel")
    ind_id <- 0
    results <- data.frame()
    
    # LOOPING Domain_Confing : Combo_df : Platform : yoy_datapoints --------------------------------------------
    
    for (metricname in metricnames){

      metrics <- dbreportedvalues %>% select("Start_date","End_date",metricname)
      metricid <- dbmetricslist[dbmetricslist$Metric==metricname,"id"]
      depvar <- metrics[,c("Start_date", metricname)]
      print("--------------------------------")
      print(paste0("Metric currently working on ",metricname))
      for(i in 1:nrow(combdf)){
        dl = combdf[i,"combo"][[1]]
        print(paste0(paste0(dl,collapse = ";")," : remaining combinations to work on: ",nrow(combdf)-i))
        
        for(platform in c('total','desktop')){
          tryCatch({
            
            for(j in 1:length(dl)){
              domain_id <- get_domain_id(traffic_con,dl[j])
              engagement_j <- get_visits_table(traffic_con,domain_id,platform)
              date_seq <- data.frame("date" = seq(engagement_j$date[1],engagement_j$date[nrow(engagement_j)],by = "1 month"))
              engagement_j <- left_join(date_seq,engagement_j,by = "date")
              engagement_j_m_pageviews <- engagement_j %>% select(date,pageviews)
              engagement_j_m_visits <- engagement_j %>% select(date,visits)
              colnames(engagement_j_m_pageviews) <- c("month","page_activity")
              colnames(engagement_j_m_visits) <- c("month","page_activity")
              
              mr <- left_join(x = engagement_j,y = tom,by = "date")
              firstdate <- mr %>% arrange(date) %>% dplyr::filter(!is.na(quarterlabel)) %>% slice(1)
              engagement_j <- m_to_q(mr,firstdate$date)
              eng_j_visits <- engagement_j %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(visits))
              eng_j_pageviews <- engagement_j %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(pageviews))
              
              if (j==1){
                eng_visits <- eng_j_visits
                eng_pageviews <- eng_j_pageviews
                eng_m_visits <- engagement_j_m_visits
                eng_m_pageviews <- engagement_j_m_pageviews
              }else{
                
                eng_visits <- add_newdomain_data(eng_visits,eng_j_visits,"quarter")
                eng_pageviews <- add_newdomain_data(eng_pageviews,eng_j_pageviews,"quarter")
                eng_m_visits <- add_newdomain_data(eng_m_visits,engagement_j_m_visits,"month")
                eng_m_pageviews <- add_newdomain_data(eng_m_pageviews,engagement_j_m_pageviews,"month")
                
              }
            } 
            
            ### SCALING TRAFFIC TO MAKE IT COMPARABLE TO METRIC AND NOT GET SLOPE IN 
            eng_pageviews$page_activity <- eng_pageviews$page_activity/10000
            eng_visits$page_activity <- eng_visits$page_activity/10000
            ######################################################################################################### 
            res <- do_reg(eng_pageviews,depvar,"yoyper",platform,"pageviews","asitis",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"pageviews","asitis",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            ######################################################################################################### 
            res <- do_reg(eng_pageviews,depvar,"yoydiff",platform,"pageviews","asitis",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"pageviews","asitis",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            ######################################################################################################### 
            res <- do_reg(eng_visits,depvar,"yoyper",platform,"visits","asitis",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"visits","asitis",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            ######################################################################################################### 
            res <- do_reg(eng_visits,depvar,"yoydiff",platform,"visits","asitis",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"visits","asitis",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            ######################################################################################################### 
            res <- do_reg(eng_pageviews,depvar,"yoyper",platform,"pageviews","rollmean",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"pageviews","rollmean",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            ######################################################################################################### 
            res <- do_reg(eng_pageviews,depvar,"yoydiff",platform,"pageviews","rollmean",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"pageviews","rollmean",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            ######################################################################################################### 
            res <- do_reg(eng_visits,depvar,"yoyper",platform,"visits","rollmean",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"visits","rollmean",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            ######################################################################################################### 
            res <- do_reg(eng_visits,depvar,"yoydiff",platform,"visits","rollmean",metricname,dl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"visits","rollmean",metricname,dl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("T_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            #########################################################################################################
            
          },error = function(err){
            print(paste0("Error with domainlist ",paste0(dl,collapse = ";")," : ", err) )  })
          
        }
        
      }
      
    }
    
    
    # CLEANING RESULTS FILE ----------------------------------------------------------------------
    
    if(nrow(results)>0){
      results$keywordlist <- NA
      loopresults <- results %>% mutate_if(is.numeric,round,digits=4) %>% select(1,2,3,6,7,10,11,12,13,14,15)
      results_reactive <- results
      output$subdomainloopresults <- renderDataTable(loopresults)
    }else{
      output$subdomainloopresults <- renderDataTable(data.frame("Message" = paste0(ticker," no models in the list")))
    }
    
    
    
    
  })
  
  observeEvent(input$submit4,{
    
    # GETTING INPUT PARAMETERS --------------------------------------------------------------------
    ticker <- input$companyticker
    domain <- input$keyworddomain
    keywordlist <- input$keywordlist
    domain_id <- get_domain_id(traffic_con,domain)
    
    # GETTING DATA FROM FUNDAMENTALS DATABASE --------------------------------------------------------------------
    
    companyinfo <- get_comapny_info(fund_con,ticker)
    dbmetricslist <- get_metric_list(fund_con,companyinfo$id)
    dbreportedvalues <- metric_long_to_wide(get_all_reported_metrics(fund_con,companyinfo$id),dbmetricslist)
    quarterlist <- dbreportedvalues %>% select("Quarter","Start_date","End_date")
    metricnames <- dbmetricslist$Metric[dbmetricslist$FT_flag==1]
    
    # CREATING DOMAIN COMBINATIONS --------------------------------------------------------------------
    
    combinations <- 1
    keywordlist <- unlist(strsplit(keywordlist, ";"))
    comb_list <- list()
    if(combinations == 1){
      for (i in 1:length(keywordlist)){
        combos <- combn(keywordlist, i, simplify = FALSE)
        comb_list <- c(comb_list, combos)
      }
      combdf <- data.frame(combo = matrix(comb_list))
    }else{
      combdf <- data.frame(combo = matrix(keywordlist))
    }
    
    # SETTING IDS FOR LOOPING --------------------------------------------------------------------
    
    tom <- quarterlist %>% select("Start_date","Quarter")
    colnames(tom) <- c("date","quarterlabel")
    ind_id <- 0
    results <- data.frame()
    
    # LOOPING Domain_Confing : Platform : Combo_df : yoy_datapoints --------------------------------------------
    
    for (metricname in metricnames){
      # Selecting the metric based on domain_config
      
      metrics <- dbreportedvalues %>% select("Start_date","End_date",metricname)
      metricid <- dbmetricslist[dbmetricslist$Metric==metricname,"id"]
      depvar <- metrics[,c("Start_date", metricname)]
      print(paste0("Metric currently working on ",metricname))
      
      for(platform in c("total","desktop")){
        
        engagement <- get_visits_table(traffic_con,domain_id,platform)
        date_seq <- data.frame("date" = seq(engagement$date[1],engagement$date[nrow(engagement)],by = "1 month"))
        engagement <- left_join(date_seq,engagement,by = "date")
        mr <- left_join(x = engagement,y = tom,by = "date")
        firstdate <- mr %>% dplyr::filter(!is.na(quarterlabel)) %>% slice(1)
        engagement <- m_to_q(mr,firstdate$date)
        pp_data <- get_pp_table(traffic_con,domain_id,platform)
        pp_data <- merge(pp_data,engagement,by = 'date')
        pp_data$visits <- pp_data$visits * pp_data$share
        pp_data$pageviews <- pp_data$pageviews * pp_data$share
        
        for (j in 1:nrow(combdf)){
          
          tryCatch({
            
            kl <- combdf[j,"combo"][[1]]
            print(paste0("Combination working on ",paste0(kl,collapse = ";")))
            pp_data[,'iskeyword'] <- as.logical(lapply(pp_data$pg_url, chkFUN, kl))
            
            pp_q <- pp_data %>% filter(iskeyword == T) %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(visits))
            pp_q$page_activity[is.na(pp_q$page_activity)] <- 0
            pp_m <- pp_data %>% filter(iskeyword == T) %>% group_by(date) %>% dplyr::summarize(page_activity = sum(visits))
            setnames(pp_m,"date","month")
            pp_q$page_activity <- pp_q$page_activity/10000
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoyper",platform,"visits","asitis",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"visits","asitis",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoydiff",platform,"visits","asitis",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"visits","asitis",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoyper",platform,"visits","rollmean",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"visits","rollmean",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoydiff",platform,"visits","rollmean",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"visits","rollmean",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.40){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
            pp_q <- pp_data %>% filter(iskeyword == T) %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(pageviews))
            pp_q$page_activity[is.na(pp_q$page_activity)] <- 0
            pp_m <- pp_data %>% filter(iskeyword == T) %>% group_by(date) %>% dplyr::summarize(page_activity = sum(visits))
            setnames(pp_m,"date","month")
            pp_q$page_activity <- pp_q$page_activity/10000
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoyper",platform,"pageviews","asitis",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"pageviews","asitis",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoydiff",platform,"pageviews","asitis",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"pageviews","asitis",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoyper",platform,"pageviews","rollmean",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoyper",platform,"pageviews","rollmean",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
            res <- do_reg(pp_q,depvar,"yoydiff",platform,"pageviews","rollmean",metricname,kl)
            if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
              model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
              res[1][[1]]$model_id = model_id
              results <- rbind(results, res[1][[1]])
              ind_id = ind_id + 1
            }
            while(res[1][[1]]$numpts.yoy > 4){
              res <- just_reg(res[2][[1]][2:nrow(res[2][[1]]),],res[3][[1]],"yoydiff",platform,"pageviews","rollmean",metricname,kl)
              if(res[[1]]$xcoeff.yoy>0 && res[[1]]$xcoeff.level>0 && res[[1]]$rsq.yoy > 0.4 && res[[1]]$rsq.level >0.4){
                model_id <- paste0("K_",companyinfo$Ticker,"_",ind_id)
                res[1][[1]]$model_id = model_id
                results <- rbind(results, res[1][[1]])
                ind_id = ind_id + 1
              }
            }
            
            #=========================================================================================================
            
          }, error = function(err){print(paste0("Error with ",paste0(kl,collapse = ";")))
          })
        }
        
      }
    } 
    
    # CLEANING RESULTS FILE ----------------------------------------------------------------------
    if(nrow(results)>0){
      setnames(results,"domainlist","keywordlist")
      results$domainlist <- domain
      loopresults <- results %>% mutate_if(is.numeric,round,digits=4) %>% select(1,2,3,6,7,10,11,12,13,14,15)
      results_reactive <- results
      output$keywordloopresults <- renderDataTable(loopresults)
    }else{
      output$keywordloopresults <- renderDataTable(data.frame("Message" = paste0(ticker," - no  models in the mongo database")))
    }
    
    
    
  })
  
  ### For Tab4 -----------------------------------------------------------------------------------------------
  
  
  md_tables <- reactiveValues()
  
  observeEvent(input$submit5,{
    
    results <- results_reactive
    # GETTING INPUT PARAMETERS -----------------------------------------------------------
    ticker <- input$modelticker
    model_id <- input$model_config_id
    model_config_id <- input$model_config_id_final
    for_client <- input$for_client
    forward_tracked <- input$forward_tracked
    logic <- input$modellogic
    category <- input$category
    dp <- results[results$model_id == model_id,]
    
    if(nrow(dp)!=1){
      output$databasestatus <- renderText(paste0("Enter a valid config_id.",model_id," not in the results list for ",ticker))
      output$yoyxy <- renderPlot(ggplot())
      output$levelxy <- renderPlot(ggplot())
      output$regxylevel <- renderPlot(ggplot())
      output$regxyyoy <- renderPlot(ggplot())
    }else{
      
      companyinfo <- get_comapny_info(fund_con,ticker)
      dbmetricslist <- get_metric_list(fund_con,companyinfo$id)
      dbreportedvalues <- metric_long_to_wide(get_all_reported_metrics(fund_con,companyinfo$id),dbmetricslist)
      quarterlist <- dbreportedvalues %>% select("Quarter","Start_date","End_date")
      metricname <- as.character(dp$metric)
      tom <- quarterlist %>% select("Start_date","Quarter")
      colnames(tom) <- c("date","quarterlabel")
      metrics <- dbreportedvalues %>% select("Start_date","End_date",metricname)
      metricid <- dbmetricslist[dbmetricslist$Metric==metricname,"id"]
      
      
      #### TABLE MODEL_SPECIFICATION
      model_specification <- data.frame(model_config_id = model_config_id,company_id = companyinfo$id,metric_id = metricid,
                                        for_client = for_client,forward_tracked = forward_tracked,
                                        category = category, logic = logic, not_forward_tracked_reason = NA)
      if(forward_tracked==1){
        model_specification$not_forward_tracked_reason <- 'Model Active'
      }else{
        model_specification$not_forward_tracked_reason <- 'Model Broke'
      }
      
      #### TABLE MODEL_DOMAIN_LIST
      domainlist <- unlist(strsplit(as.character(dp$domainlist),";"))
      keywordlist <- unlist(strsplit(as.character(dp$keywordlist),";"))
      domain_ids <- c()
      for (d in domainlist){
        domain_ids <- c(domain_ids,get_domain_id(traffic_con,d))
      }
      
      model_domain_list <- data.frame(model_config_id = model_config_id,
                                      domain_id = domain_ids,
                                      keyword = keywordlist,
                                      yoy_transformation = yoy_transform_client_db_versions(dp$transform), 
                                      level_transformation = level_transform_client_db_versions(dp$leveltransform),
                                      platform = platform_client_db_versions(dp$platform),
                                      traffic_type = form_client_db_versions(dp$form)
                                      )
      
      
      ### CALCULATIONS FOR THE REST OF THE TABLES
      
      if(aggregate(model_domain_list$keyword,list(model_domain_list$domain_id),paste,collapse = "")$x == "NA"){
        indvar <- get_indvar_T(traffic_con,model_domain_list,tom)
      }else{
        indvar <- get_indvar_K(traffic_con,model_domain_list,tom)
      }
      
      depvar <- metrics[,c("Start_date", metricname)]
      indvar = level_transform(indvar,dp$leveltransform)
      if (dp$leveltransform == '4-QMA-B'){
        colnames(depvar) <- c('Start_date','page_activity')
        depvar <- level_transform(depvar,dp$leveltransform)
        colnames(depvar) <- c('Start_date','Reported_value')
      }
      yoytransform_data <- yoy_transform(indvar,depvar,dp$transform)
      vars_output <- csv_vars_output(yoytransform_data[1][[1]],yoytransform_data[2][[1]])
      vars <- vars_output[3][[1]]
      csv_vars <- vars_output[1][[1]]
      
      res <- just_reg(vars[(nrow(vars)-dp$numpts.yoy+1):nrow(vars),],csv_vars,dp$transform,'platform','form',dp$leveltransform,"Reported_value",'domainlist')
      csv_vars <- raw_data_standardizing(res[[3]],quarterlist)
      
      if (!is.na(csv_vars$Metric[nrow(csv_vars)-1]) & is.na(csv_vars$Metric[nrow(csv_vars)])){
        
        modeloutput <- dp$intcoeff.yoy + dp$xcoeff.yoy*csv_vars$YOY.ind[nrow(csv_vars)]
        
        if (as.character(dp$transform) == "yoyper"){ ### YOY - PREDICTION
          
          yoy_estimate <- modeloutput
          
        }else{ ### YOY-D PREDICTION
          
          yoy_estimate <- modeloutput/csv_vars[nrow(csv_vars)-4,]$Metric
          
        }
        
        level_estimate <- csv_vars$Metric[nrow(csv_vars)-4]*(1 + yoy_estimate)
      }else{
        
        modeloutput <- NA
        level_estimate <- NA
        yoy_estimate <- NA
        
      }
      
      
      #### TABLE MODEL_CURRENT_QUARTER
      
      model_current_quarter <- data.frame(model_config_id = model_config_id, current_quarter_label = csv_vars$quarterlabel[nrow(csv_vars)],
                                          current_end_date = csv_vars$End_date[nrow(csv_vars)])
      
      #### TABLE MODEL_DESCRIPTION
      
      model_description <- data.frame(model_config_id = model_config_id, quarter_label = csv_vars$quarterlabel[nrow(csv_vars)],
                                      end_date = csv_vars$End_date[nrow(csv_vars)],
                                      data_points = res[[1]]$numpts.yoy, rsq = res[[1]]$rsq.yoy,
                                      intercept = res[[1]]$intcoeff.yoy, gradient = res[[1]]$xcoeff.yoy)
      
      
      #### TABLE MODEL_ERROR_LIST
      
      model_error_rate <- data.frame(model_config_id = model_config_id, 
                                     quarter_label = csv_vars$quarterlabel[nrow(csv_vars)],
                                     end_date = as.Date(csv_vars$End_date[nrow(csv_vars)]),
                                     model_output = modeloutput, 
                                     yoy_estimate = yoy_estimate, 
                                     level_estimate = level_estimate)
      if (for_client == 1){
        model_error_rate$client_historical = 1
      }else{
        model_error_rate$client_historical = 0
      }
      
      #### TABLE MODELS_RAW_DATA
      
      
      model_raw_data <- csv_vars
      model_raw_data$model_config_id <- model_config_id
      model_raw_data <- model_raw_data %>% select(model_config_id,Date,End_date,quarterlabel,
                                                  Traffic,Metric, YOY.ind,YOY.dep)
      colnames(model_raw_data) <- c("model_config_id","start_date","end_date","quarter_label",
                                    "tomahawk_metric","company_metric","tomahawk_yoy","company_yoy")
      
      #### FOR PLOTS
      vars <- model_raw_data[5:nrow(model_raw_data),c("end_date","tomahawk_yoy","company_yoy")]
      output$regxylevel <- renderPlot(ggplotRegression(lm(company_metric ~ tomahawk_metric, data = model_raw_data)))
      output$regxyyoy <- renderPlot(ggplotRegression(lm(company_yoy ~ tomahawk_yoy, data = vars)))
      scale = max(vars$company_yoy, na.rm = T)/max(vars$tomahawk_yoy, na.rm = T)
      py <- ggplot(vars, aes(x = end_date)) +
        geom_line(aes(y = tomahawk_yoy, colour = "YoY Traffic"),size = 1) +
        geom_line(aes(y = company_yoy, colour = "YoY Metric"),size = 1) +
        geom_point(aes(y = tomahawk_yoy, colour = "YoY Traffic"),size = 2) +
        geom_point(aes(y = company_yoy, colour = "YoY Metric"),size = 2) +
        #scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "YoY Metric")) + 
        theme(legend.position="bottom") + labs(y = "YoY Traffic",x = "Date",colour = "Variables")
      py
      output$yoyxy <- renderPlot(py)
      
      scale = max(model_raw_data$company_metric, na.rm = T)/max(model_raw_data$tomahawk_metric, na.rm = T)
      pl <- ggplot(model_raw_data,aes(x = end_date)) + 
        geom_line(aes(y = tomahawk_metric,colour = "Traffic"),size = 1) + 
        geom_line(aes(y = company_metric/scale,colour = "Metric"),size = 1) + 
        geom_point(aes(y = tomahawk_metric,colour = "Traffic"),size = 2) +
        geom_point(aes(y = company_metric/scale,colour = "Metric"),size = 2) + 
        scale_y_continuous(sec.axis = sec_axis(~.*scale,name = "Metric")) + 
        theme(legend.position = "bottom") + labs(colour = "Variables")
      pl
      output$levelxy <- renderPlot(pl)
    
      #### ADDING THE TABLES TO REACTIVE VALUE
      
      md_tables$model_specification <- model_specification
      md_tables$model_description <- model_description
      md_tables$model_current_quarter <- model_current_quarter
      md_tables$model_error_rate <- model_error_rate
      md_tables$model_raw_data <- model_raw_data
      md_tables$model_domain_list <- model_domain_list
      
      }
    
      
  })
  
  ### PUSHING A NEW MODEL TO THE DATABASE
  
  observeEvent(input$submit6,{
  
    
    model_specs <- check_model_in_database(model_con,model_config_id)
    if(nrow(model_specs)==1){
      output$databasestatus <- renderText(paste0("Database already has a model with id:",model_config_id," for ",ticker))
    }else{
      if(nrow(md)==0){
        output$databasestatus <- renderText(paste0("Enter a valid config_id.",model_config_id," not in mongo database for ",ticker))
      }else{
        
        
        model_specification <- md_tables$model_specification
        model_description <- md_tables$model_description
        model_current_quarter <- md_tables$model_current_quarter
        model_error_rate <- md_tables$model_error_date
        model_raw_data <- md_tables$model_raw_data
        model_domain_list <- md_tables$model_domain_list
        
        
        write_database(model_specification,"model_specification",model_con)
        write_database(model_description,"model_description",model_con)
        write_database(model_current_quarter,"model_current_quarter",model_con)
        if(!is.na(model_error_rate$model_output)){
          write_database(model_error_rate,"model_error_rate",model_con) 
        }
        write_database(model_raw_data,"model_raw_data",model_con)
        write_database(model_domain_list,"model_domain_list",model_con)
        output$databasestatus <- renderText(paste0("Model updated for ",model_config_id,", ",ticker))
      }

    }
   
  })
    
  
 
  
})    
    
    
    
