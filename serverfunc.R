
################################### FUNDAMENTAL MODELS SERVER FUNC FILE ###################################

########################## Fundamentals Database connections ##################################################
get_comapny_id<-function(con,companyName,ticker){
  if (ticker==""){
    query<-paste0("select id from Company where Name = '",companyName, "';")
    companyid<-dbGetQuery(con,query);
    
  }else{
    query<-paste0("select id from Company where Ticker = '",ticker, "';")
    companyid<-dbGetQuery(con,query);
  }
  
  if (nrow(companyid)==0){ #if it doesn't exist in the DB, return an error
    print(paste("Data missing for  ",companyName, sep=""))
    return("No Data")
  } else if (nrow(companyid)==1){
    return(companyid[1,1])
  }
}

get_comapny_info<-function(con,ticker){
    query<-paste0("select * from Company where Ticker = '",ticker, "';")
    companyid<-dbGetQuery(con,query);
    return(companyid)
}

get_comapny_info1<-function(con,companyid){
  query<-paste0("select * from Company where id = ",companyid, ";")
  companyinfo<-dbGetQuery(con,query);
  return(companyinfo)
}

get_metric_list <- function(con,companyid){
  query<-paste0("select * from Metric where Company_id = ",companyid, ";")
  dbmetricslist<-dbGetQuery(con,query)
  if(nrow(dbmetricslist)>0){
    return(dbmetricslist)
  }else{return("No MetricNames for this Company in Database")}
}

get_reported_metrics <- function(con,companyid,metricid){
  query<-paste0("select * from Reported where Company_id = ",companyid," and Metric_id = ",metricid, " order by Start_date;")
  dbreportedvalues<-dbGetQuery(con,query)
  if(nrow(dbreportedvalues)>0){
    return(dbreportedvalues)
  }else{return("No Reported Metric for selected company and metric")}
  
}

get_all_reported_metrics <- function(con,companyid){
  query<-paste0("select * from Reported where Company_id = ",companyid," order by Start_date;")
  dbreportedvalues<-dbGetQuery(con,query)
  if(nrow(dbreportedvalues)>0){
    return(dbreportedvalues)
  }else{return("No Reported Metric for selected company and metric")}
  
}

get_all_earnings <- function(con){
  query <- "select Name,Ticker,Quarter,Earning_Date from Earningdates a
            join Company b on a.Company_id = b.id;"
  dbearnings<-dbGetQuery(con,query);
  return(dbearnings)
}

get_earnings_company <- function(con,companyid){
  query <- paste0("select Name,Ticker,Quarter,Earning_Date from Earningdates a join Company b on a.Company_id = b.id where b.id = ",companyid, ";")
  dbearnings<-dbGetQuery(con,query);
  return(dbearnings)
}

get_all_consensus <- function(con){
  query <- "select Name, Ticker, Metric, Quarter, Consensus from Consensus a
            join Metric b on a.Metric_id = b.id join Company c on a.Company_id = c.id;"
  dbconsensus<- dbGetQuery(con,query);
  return(dbconsensus)
}

get_consensus_company <- function(con,companyid){
  query <- paste0("select Name, Ticker, Metric, Quarter, Consensus from Consensus a join Metric b on a.Metric_id = b.id join Company c on a.Company_id = c.id where c.id =",companyid, ";")
  dbconsensus<- dbGetQuery(con,query);
  return(dbconsensus)
}

get_all_company_list <- function(con){
  query<-paste0("select * from Company;")
  dbmaincompanylist<-dbGetQuery(con,query);
  return(dbmaincompanylist)
}

################################################################## Traffic Database Connections ##############################################
get_domain_id<-function(con,domain){
  
  query<-paste0("select domain_id from strat_new.domains where domain = '", domain, "';")
  domain_id<-dbGetQuery(con,query);
  if (nrow(domain_id)==0){ #if it doesn't exist in the DB, return an error
    print(paste("Data missing for  ",domain, sep=""))
    return("No Data")
  } else if (nrow(domain_id)==1){
    return(domain_id[1,1])
  }
}

get_groups <- function(con){
  query <- 'select * from groups;'
  groups <- dbGetQuery(con,query)
  return(groups)
}

get_maindomain_table<- function(con,domain,platform){
      
      query = paste0('select domain_id,max(date) from traffic_visits where domain_id in 
                      (select domain_id from domains where main_domain = "',domain,'");')
      domains_table <- dbGetQuery(con,query);
      return(domains_table)
}

get_domain_dbdetails<- function(con,domain){
  query = paste0('select domain,platform,min(date),max(date) from traffic_visits a
                 join domains b on a.domain_id = b.domain_id
                 where granularity = "Monthly" and 
                 b.domain = "',domain,'"group by platform;')
  traf_dbdetails <- dbGetQuery(con,query);
  
   query = paste0('select domain,platform,min(date),max(date) from popular_pages a
                 join domains b on a.domain_id = b.domain_id
                 where b.domain = "',domain,'"group by platform;')
  pp_dbdetails <- dbGetQuery(con,query);
  return(list(traf_dbdetails,pp_dbdetails))
}

get_domain_id_dbdetails<- function(con,domain_id){
  query = paste0('select domain,platform,min(date),max(date) from traffic_visits a
                 join domains b on a.domain_id = b.domain_id
                 where granularity = "Monthly" and 
                 b.domain_id = ',domain_id,' group by platform;')
  traf_dbdetails <- dbGetQuery(con,query);
  
  query = paste0('select domain,platform,min(date),max(date) from popular_pages a
                 join domains b on a.domain_id = b.domain_id
                 where b.domain_id = ',domain_id,' group by platform;')
  pp_dbdetails <- dbGetQuery(con,query);
  return(list(traf_dbdetails,pp_dbdetails))
}

get_visits_table <- function(con,domain_id,platform){
  
  query = paste0("select date,visits from traffic_visits where domain_id = ",domain_id, " and platform = '", platform,
                 "' and granularity = 'Monthly' order by date;")
  traffic<-dbGetQuery(con,query);
  query = paste0("select date,pages_per_visit from traffic_ppv where domain_id = ", domain_id,
                 " and platform = '",platform,"' and granularity = 'Monthly' order by date;" )
  ppv<-dbGetQuery(con,query);
  pv <- left_join(x = traffic, y = ppv, by = c("date"))
  pv <- pv %>% filter(!is.na(visits)) %>% filter(!is.na(pages_per_visit))
  pv <- pv %>% mutate(pageviews = visits*pages_per_visit) %>% select(-pages_per_visit)
  pv$date <- as.Date(pv$date, format = "%Y-%m-%d")
  return(pv)
  
}

get_pp_table <- function(con,domain_id,platform){
  
  query = paste0("select date,pg_url,share from popular_pages where domain_id = ",domain_id," and platform = '", platform, "';") 
  pp_data<-dbGetQuery(con,query);
  pp_data$date <- as.Date(pp_data$date, format = "%Y-%m-%d")
  pp_data <- pp_data %>% arrange(date, desc(share))
  return(pp_data)
  
}

#################################### Dealing with keywords and subdomains ##########################################################

chkFUN <- function(x, kwdlist){
  #x as string to check
  return(any(sapply(kwdlist, grepl, str_to_lower(x))))
}

get_combo_cols <- function(domain_id, platform, combo,engagement,con){
  
  pp_data <- get_pp_table(con,domain_id,platform)
  pp_data <- merge(pp_data,engagement,by = 'date')
  pp_data$visits <- pp_data$visits * pp_data$share
  pp_data$pageviews <- pp_data$pageviews * pp_data$share
  pp_data[,'iskeyword'] <- as.logical(lapply(pp_data$pg_url, chkFUN, combo))
  return(pp_data)
}

get_topk_keywords <- function(domain,pp_data){
  
  setDT(pp_data)[,"keywords" := gsub("/",", ",gsub("[&%.?=-]","/",gsub(domain,"",tolower(pg_url))))]
  kf <- data.frame(words = unlist(strsplit(tolower(pp_data$keywords), ", "))) %>% group_by(words) %>% 
    dplyr::summarise(n = n()) %>% arrange(desc(n))
  setDT(kf)[,"Len" := nchar(as.character(words))]
  kf <- subset(kf,Len>1 & Len<25)
  return(kf)
}

get_subdomain_info <- function(pp_data){
  
  setDT(pp_data)[, "firstfolder" := tstrsplit(pg_url, "/")[1]]
  firstfoldercounts <- data.table(table(pp_data$firstfolder))
  names(firstfoldercounts) = c("firstfolder","frequency")
  firstfoldercounts <- firstfoldercounts[order(-frequency),]
  return(firstfoldercounts)
}
############################ Dealing with quarters and months #############################################################

m_to_q <- function(mr,qmonth){
  
  myrow <- which(grepl(qmonth, mr$date))
  
  #go forward
  count <- 1
  fwdq <- qmonth
  for (i in which(mr$date == qmonth):nrow(mr)){
    mr[i,c("quarter")] <- fwdq
    count <- count + 1
    if (count == 4){
      fwdq <- fwdq %m+% months(3)
      count <- 1
    }
  }
  
  #go backward
  if (myrow > 1){
    count <- 1
    bwdq <- qmonth %m-% months(3)
    for (i in (myrow-1):1) {
      mr[i, c("quarter")] <- bwdq
      count <- count + 1
      if (count == 4){
        bwdq <- bwdq %m-% months(3)
        count <- 1
      }
    }
  }
  
  #remove instances where there are less than 3 months:
  q_agg <- mr %>% group_by(quarter) %>% dplyr::summarize(n = n())
  mr$quarter[which(mr$quarter %in% (q_agg$quarter[which(q_agg$n < 3)]))] <- NA
  q_agg <- q_agg %>% filter(n == 3) %>% select(-n) #this is final set of quarters
  mr <- mr %>% filter(!is.na(quarter)) %>% select(-quarterlabel)

  return(mr)
}

q_of_domain <- function(domain,sel_keywords,platform,form,metrics,con){
  
  domain_id <- get_domain_id(con,domain)
  if (class(domain_id)=="integer"){
    tom <- metrics[,c(1,2)]
    colnames(tom) <- c("date","q")
    
    engagement <- get_visits_table(con,domain_id,platform)
    engagement <- engagement[order(engagement$date),] 
    eng <- engagement
    pp_data <- get_pp_table(con,domain_id,platform)
    pp_data <- merge(pp_data,engagement,by = 'date')
    pp_data$visits <- pp_data$visits * pp_data$share
    pp_data$pageviews <- pp_data$pageviews * pp_data$share
    
    mr <- left_join(x = engagement,y = tom,by = "date")
    firstdate <- mr %>% dplyr::filter(!is.na(q)) %>% slice(1)
    qmonth <- firstdate$date
    engagement <- m_to_q(mr,qmonth)
    
    
    
    if (sel_keywords != ""){
      sel_keywords <- unlist(strsplit(sel_keywords, ";"))
      pp_data[,'iskeyword'] <- as.logical(lapply(pp_data$pg_url, chkFUN, sel_keywords))
      setnames(pp_data,form,"page_activity")
      pp_m <- pp_data %>% filter(iskeyword == T) %>% 
        group_by(date) %>% dplyr::summarize(page_activity = sum(page_activity))
      pp_data <- merge(pp_data,engagement[,c("date","quarter")],by = "date")
      pp_q <- pp_data %>% filter(iskeyword == T) %>% 
        group_by(quarter) %>% dplyr::summarize(page_activity = sum(page_activity))
      
    }else{
      setnames(engagement,form,"page_activity")
      pp_q <- engagement %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(page_activity))
      setnames(eng,form,"page_activity")
      pp_m <- eng %>% group_by(date) %>% dplyr::summarize(page_activity = sum(page_activity))
      
    }
    pp_q$page_activity[is.na(pp_q$page_activity)] <- 0
    pp_m$page_activity[is.na(pp_m$page_activity)] <- 0
    return(list(pp_q,pp_m))
    
  }else{
    return("Enter Correct Domain")
  }
}


## CLEAN
m_of_domain <- function(con,mdl){
  if(aggregate(mdl$keyword,list(mdl$domain_id),paste,collapse = ";")$x=="NA"){
    for (d in 1:nrow(mdl)){
      domain_id <- mdl$domain_id[d]
      platform <- platform_client_db_versions(mdl$platform[d])
      form <- form_client_db_versions(mdl$traffic_type[d])
      engagement <- get_visits_table(con,domain_id,platform)
      engagement <- engagement[order(engagement$date),]
      setnames(engagement,tolower(form),"page_activity")
      pp_m <- engagement[,c("date","page_activity")]
      if(d==1){
        eng_m <- pp_m
      }else{
        eng_m <- add_newdomain_data(eng_m,pp_m,"date")
      }
    }
  }else{
    keywordlist <- aggregate(mdl$keyword,list(mdl$domain_id),paste,collapse = ";")$x
    keywordlist <- unlist(strsplit(keywordlist, ";"))
    domain_id <- aggregate(mdl$keyword,list(mdl$domain_id),paste,collapse = ";")$Group.1
    platform <- platform_client_db_versions(unique(mdl$platform))
    form <- form_client_db_versions(unique(mdl$traffic_type))
    engagement <- get_visits_table(con,domain_id,platform)
    pp_data <- get_pp_table(con,domain_id,platform)
    pp_data <- merge(pp_data,engagement,by = 'date')
    pp_data["page_activity"] = pp_data[form] * pp_data["share"]
    pp_data[,'iskeyword'] <- as.logical(lapply(pp_data$pg_url, chkFUN, keywordlist))
    eng_m <- pp_data %>% filter(iskeyword == T) %>% 
      group_by(date) %>% dplyr::summarize(page_activity = sum(page_activity))
    
  }
  return(eng_m)
}

add_newdomain_data <- function(df,dfj,by){
  df = left_join(df,dfj,by = by,suffix = c(".1",".2"))
  df$page_activity <- df$page_activity.1 + df$page_activity.2
  df <- df %>% select(by,'page_activity')
  return(df)
}

domain_add <- function(d,d1,by){
  setnames(d1,"page_activity","page_activity1")
  d <- merge(d,d1,by = by,all = T)
  d$page_activity <- d$page_activity + d$page_activity1
  d <- d %>% select(by,'page_activity')  
}

create_quarter_end <- function(Start){
  End <- as.Date(Start)
  month(End) <- month(as.Date(Start)) + 2
  day(End) <- days_in_month(End)
  return(End)
}

create_quarterlabel_list <- function(startQuarterLabel,n,track = "forward"){
  if(track=="forward"){
    qtostart = c(startQuarterLabel)
    for (i in 2:n){
      lk = unlist(strsplit(qtostart[i-1], "-"))
      q <- strtoi(substr(lk[1],2,2))
      if(q<4){
        q = q+1
        y <- strtoi(substr(lk[2],2,5))
        qtostart = c(qtostart,paste0("Q",q,"-Y",y))
      }else{
        q = 1
        y <- strtoi(substr(lk[2],2,5)) + 1
        qtostart = c(qtostart,paste0("Q",q,"-Y",y))
      }
      
    }
  }else{
    qtostart = c(startQuarterLabel)
    for (i in 2:n){
      lk = unlist(strsplit(qtostart[i-1], "-"))
      q <- strtoi(substr(lk[1],2,2))
      if(q>1){
        q = q-1
        y <- strtoi(substr(lk[2],2,5))
        qtostart = c(qtostart,paste0("Q",q,"-Y",y))
      }else{
        q = 4
        y <- strtoi(substr(lk[2],2,5)) - 1
        qtostart = c(qtostart,paste0("Q",q,"-Y",y))
      }
      
    }
    qtostart <- rev(qtostart)
  }
  
  return(qtostart)
}

create_empty_matrixdf <- function(quarterStartDateSeries,qtostart,metricName){
  df <- data.frame(quarterStartDateSeries,qtostart)
  colnames(df) <- c("Start","Quarter")
  df$Start <- as.character(df$Start)
  df[metricName] <- NA 
  return(df)
  
}

## CLEAN
get_indvar_T <- function(con,mdl,tom){
  for(j in 1:nrow(mdl)){
    domain_id <- mdl$domain_id[j]
    platform <- platform_client_db_versions(mdl$platform[j])
    form <- form_client_db_versions(mdl$traffic_type[j])
    leveltransform <- level_transform_client_db_versions(mdl$level_transformation[j])
    yoytransform <- yoy_transform_client_db_versions(mdl$yoy_transformation[j])
    engagement_j <- get_visits_table(con,domain_id,platform)
    date_seq <- data.frame("date" = seq(engagement_j$date[1],engagement_j$date[nrow(engagement_j)],by = "1 month"))
    engagement_j <- left_join(date_seq,engagement_j,by = "date")
    mr <- left_join(x = engagement_j,y = tom,by = "date")
    firstdate <- mr %>% arrange(date) %>% dplyr::filter(!is.na(quarterlabel)) %>% slice(1)
    engagement_j <- m_to_q(mr,firstdate$date)
    if(form=="visits"){
      eng_j <- engagement_j %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(visits))
    }else{
      eng_j <- engagement_j %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(pageviews))
    }
    
    if(j==1){
      eng_q <- eng_j
    }else{
      eng_q <- add_newdomain_data(eng_q,eng_j,"quarter")
    }
    
  }
  
  eng_q$page_activity <- eng_q$page_activity/10000
  return(eng_q)
}

## CLEAN
get_indvar_K <- function(con,mdl,tom){
  keywordlist <- aggregate(mdl$keyword,list(mdl$domain_id),paste,collapse = ";")$x
  keywordlist <- unlist(strsplit(keywordlist, ";"))
  domain_id <- aggregate(mdl$keyword,list(mdl$domain_id),paste,collapse = ";")$Group.1
  platform <- platform_client_db_versions(unique(mdl$platform))
  form <- form_client_db_versions(unique(mdl$traffic_type))
  leveltransform <- level_transform_client_db_versions(unique(mdl$level_transformation))
  yoytransform <- yoy_transform_client_db_versions(unique(mdl$yoy_transformation))
  engagement <- get_visits_table(con,domain_id,platform)
  date_seq <- data.frame("date" = seq(engagement$date[1],engagement$date[nrow(engagement)],by = "1 month"))
  engagement <- left_join(date_seq,engagement,by = "date")
  mr <- left_join(x = engagement,y = tom,by = "date")
  firstdate <- mr %>% dplyr::filter(!is.na(quarterlabel)) %>% slice(1)
  engagement <- m_to_q(mr,firstdate$date)
  pp_data <- get_pp_table(con,domain_id,platform)
  pp_data <- merge(pp_data,engagement,by = 'date')
  pp_data["page_activity"] = pp_data[form] * pp_data["share"]
  pp_data[,'iskeyword'] <- as.logical(lapply(pp_data$pg_url, chkFUN, keywordlist))
  pp_q <- pp_data %>% filter(iskeyword == T) %>% group_by(quarter) %>% dplyr::summarize(page_activity = sum(page_activity))
  pp_q$page_activity <- pp_q$page_activity/10000
  return(pp_q)
}

############################## Transformations ##############################################################

level_transform <- function(leveltable,leveltransform){
  if(leveltransform =="rollmean"){
    leveltable$page_activity <-c(NA,NA,NA,rollmean(leveltable$page_activity,k=4))
  }else if(leveltransform=="reverse"){
    leveltable$page_activity <- 1/leveltable$page_activity
  }else if(leveltransform=="log"){
    leveltable$page_activity <- log(leveltable$page_activity)
  }
  return(leveltable)
}

yoy_transform <- function(indvar,depvar,transform){
  colnames(depvar) <- c('Date', 'Metric')
  depvar <- depvar %>% arrange(Date)
  
  colnames(indvar) <- c('Date', 'Traffic')
  indvar <- indvar %>% arrange(Date)
  #print("collected indvar")
  
  if(transform == 'yoyper'){
    depvar <- depvar %>% mutate(YOY = Metric/lag(Metric,4)-1)
    indvar <- indvar %>% mutate(YOY = Traffic/lag(Traffic,4)-1)
    #print("Correctly Transformed")
  }else{
    depvar <- depvar %>% mutate(YOY = Metric - lag(Metric,4))
    indvar <- indvar %>% mutate(YOY = Traffic - lag(Traffic,4))
    #print("Correctly Transformed")
  }
  return(list(indvar,depvar))
  
}

metric_long_to_wide <- function(dbreportedvalues,dbmetricslist){
  
  if(class(dbreportedvalues)=="data.frame"){
    
    dbreportedvalues$Start_date <- date(dbreportedvalues$Start_date)
    dbreportedvalues$End_date <- date(dbreportedvalues$End_date)
    dbreportedvalues <- left_join(dbreportedvalues[,c(2,3,4,5,6)],dbmetricslist[,c("Metric","id")],by = c("Metric_id"="id"))
    dbreportedvalues <- dbreportedvalues %>% select(-Metric_id)
    dbreportedvalues <- spread(dbreportedvalues,Metric,Reported_value)
    dbreportedvalues <- dbreportedvalues[order(dbreportedvalues$Start_date),]
    return(dbreportedvalues)
  }else{
    return("No data values")
  }
  
}

csv_vars_output <- function(indvar,depvar){
  csv_vars <- left_join(x = indvar, y = depvar, by = c("Date"), suffix = c(".ind", ".dep"))
  csv_vars$YOY.ind[which(csv_vars$YOY.ind == Inf)] <- NA
  csv_vars$YOY.dep[which(csv_vars$YOY.dep == Inf)] <- NA
  varslevel <- csv_vars %>% select(Date,Traffic,Metric)
  varslevel <- varslevel %>% filter(!is.na(Traffic)) %>% filter(!is.na(Metric))
  vars <- csv_vars %>% select(Date,YOY.dep,YOY.ind)
  vars <- vars %>% filter(!is.na(YOY.ind)) %>% filter(!is.na(YOY.dep))
  return(list(csv_vars,varslevel,vars))
}

raw_data_standardizing <- function(csv_vars,quarterlist){
  csv_vars$quarterlabel <- NA
  csv_vars$End_date <- create_quarter_end(csv_vars$Date)
  last_metric_entry <- max(which(!is.na(csv_vars$Metric)))
  last_traffic_entry <- max(which(!is.na(csv_vars$Traffic)))
  current_quarter_startdate <- csv_vars[last_metric_entry,]$Date
  current_quarterlabel <- create_quarterlabel_list(quarterlist[quarterlist$Start_date == current_quarter_startdate,]$Quarter,2)[2]
  month(current_quarter_startdate) <- month(current_quarter_startdate) + 3
  current_quarter_enddate <- create_quarter_end(current_quarter_startdate)
  if(last_metric_entry==last_traffic_entry){
    
    newline <- data.frame(current_quarter_startdate,NA,NA,NA,NA,current_quarterlabel,
                          current_quarter_enddate)
    colnames(newline) <- colnames(csv_vars)
    csv_vars <- rbind(csv_vars,newline)
  }
  csv_vars$quarterlabel <- create_quarterlabel_list(current_quarterlabel,nrow(csv_vars),"backward")
  return(csv_vars)
  
}

############################### Regression Functions #################################################################

do_reg <- function(indvar,depvar,transform,platform,form,leveltransform,sel_metric,domainlist){
  
  indvar = level_transform(indvar,leveltransform)
  yoytransform <- yoy_transform(indvar,depvar,transform)
  vars_output <- csv_vars_output(yoytransform[1][[1]],yoytransform[2][[1]])
  
  vars <- vars_output[3][[1]]
  varslevel <- vars_output[2][[1]]
  regyoy <- summary(lm(YOY.dep ~ YOY.ind, vars))
  reglevel <- summary(lm(Traffic ~ Metric,varslevel))
  domainlist = paste0(domainlist,collapse = ";")
  rsq <- data.frame(domainlist,sel_metric,
                    regyoy$r.squared,regyoy$coefficients[1,c("Estimate")],
                    regyoy$coefficients[2,c("Estimate")],nrow(vars),
                    reglevel$r.squared,reglevel$coefficients[1,c("Estimate")],
                    reglevel$coefficients[2,c("Estimate")],nrow(varslevel),
                    transform,platform,form,leveltransform)
  names(rsq) = c("domainlist","metric",
                 "rsq.yoy","intcoeff.yoy","xcoeff.yoy","numpts.yoy",
                 "rsq.level","intcoeff.level","xcoeff.level","numpts.level",
                 "transform","platform","form","leveltransform")
  return(list(rsq,vars,vars_output[1][[1]]))
  
}

just_reg <- function(vars,csv_vars,transform,platform,form,leveltransform,sel_metric,domainlist){
  nd = which(csv_vars$Date==vars[1,1])-4
  regyoy <- summary(lm(YOY.dep ~ YOY.ind, vars))
  csv_vars <- csv_vars[nd:nrow(csv_vars),]
  csv_vars[1:4,]$YOY.ind <- NA
  csv_vars[1:4,]$YOY.dep <- NA
  varslevel <- csv_vars %>% select(Date,Traffic,Metric)
  varslevel <- varslevel %>% filter(!is.na(Traffic)) %>% filter(!is.na(Metric))
  reglevel <- summary(lm(Traffic ~ Metric,varslevel))
  domainlist = paste0(domainlist,collapse = ";")
  rsq <- data.frame(domainlist,sel_metric,
                    regyoy$r.squared,regyoy$coefficients[1,c("Estimate")],
                    regyoy$coefficients[2,c("Estimate")],nrow(vars),
                    reglevel$r.squared,reglevel$coefficients[1,c("Estimate")],
                    reglevel$coefficients[2,c("Estimate")],nrow(varslevel),
                    transform,platform,form,leveltransform)
  names(rsq) = c("domainlist","metric",
                 "rsq.yoy","intcoeff.yoy","xcoeff.yoy","numpts.yoy",
                 "rsq.level","intcoeff.level","xcoeff.level","numpts.level",
                 "transform","platform","form","leveltransform")
  return(list(rsq,vars,csv_vars))
  
}

do_reg_clust <- function(pp_d,depvar,form,transform,platform,sel_metric,cm){
  
  pp_q <- pp_d %>% filter(cluster %in% c(cm[1],cm[2],cm[3])) %>% 
    group_by(quarter) %>% dplyr::summarize(page_activity = sum(page_activity))
  pp_q$page_activity[is.na(pp_q$page_activity)] <- 0
  res <- as.data.frame(do_reg(pp_q,depvar,transform,platform,form,sel_metric,cm))
  return(res)
}

do_reg_W_plot <- function(depvar, indvar, transform, chartout = F, csvout = F){
  #q1month is the ending month of a Quarter 1.
  #ex if Q1Y2016 ends 3/31/2016 then q1month = 3/31/16
  #metric is a quarterly time series of the comapny's raw reported data with month-end dates
  #kwdlist is a string of keywords 
  
  colnames(depvar) <- c('Date', 'Metric')
  depvar <- depvar %>% arrange(Date)
  depvar <- depvar %>% mutate(YOY = Metric/lag(Metric,4)-1,
                              QOQ = Metric/lag(Metric,1)-1)
  
  colnames(indvar) <- c('Date', 'Traffic')
  indvar <- indvar %>% arrange(Date)
  indvar <- indvar %>% mutate(YOY = Traffic/lag(Traffic,4)-1,
                              QOQ = Traffic/lag(Traffic,1)-1)
  if (csvout == T){
    csv_vars <- left_join(x = indvar, y = depvar, by = c("Date"), suffix = c(".ind", ".dep"))
    write.csv(csv_vars, "./output/csvout.csv")
  }
  
  if (transform == "YOY"){
    depvar <- depvar %>% select(Date, YOY) %>% filter(!is.na(YOY))
    indvar <- indvar %>% select(Date, YOY) %>% filter(!is.na(YOY))
  } else if (transform == "QOQ"){
    depvar <- depvar %>% select(Date, QOQ)
    indvar <- indvar %>% select(Date, QOQ)
  }
  
  #merge:
  vars <- left_join(x = indvar, y = depvar, by = c("Date"), suffix = c(".ind", ".dep"))
  vars <- vars %>% filter(!is.na(YOY.ind)) %>% filter(!is.na(YOY.dep))
  
  #regress:
  vars$YOY.ind[which(vars$YOY.ind == Inf)] <- NA
  vars$YOY.dep[which(vars$YOY.dep == Inf)] <- NA
  numpts <- min(sum(!is.na(vars$YOY.ind) == T),sum(!is.na(vars$YOY.dep) == T))
  reg <- summary(lm(YOY.dep ~ YOY.ind, vars))
  
  if (chartout == T){
    ggplotRegression(lm(YOY.dep ~ YOY.ind, vars))
    ggsave("./output/scatter.png")
    
    scale = mean(csv_vars$Metric, na.rm = T)/mean(csv_vars$Traffic, na.rm = T)
    ggplot(csv_vars, aes(x = Date)) +
      geom_line(aes(y = Traffic, colour = "Traffic")) +
      geom_line(aes(y = Metric/scale, colour = "Metric")) +
      scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Metric"))
    ggsave("./output/levels.png")
  }
  
  return(list(rsq = reg$r.squared,
              arsq = reg$adj.r.squared,
              intcoeff = reg$coefficients[1,c("Estimate")],
              xcoeff = reg$coefficients[2,c("Estimate")],
              numpts = numpts))
  
}

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red", se = F) +
    labs(title = paste("R2 = ",signif(summary(fit)$r.squared, 5),
                       "Int =",signif(fit$coef[[1]],5 ),
                       " Slp =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

cal_model_estimate <- function(model_input,intercept,gradient,yoytransform,pre_quar_level){
  model_output <- intercept + model_input*gradient
  
  if (yoytransform == "yoyper"){ 
    yoy_estimate <- model_output
  }else{ 
    yoy_estimate <- model_output/pre_quar_level
  }  
  level_estimate <- pre_quar_level*(1 + yoy_estimate)
  
  return(c(model_output,yoy_estimate,level_estimate))
}  


############################### JSON File Output ######################################################################

mongo_document <- function(model_id,model_spec,vars,csv_vars,quarterlist,maindomain,metricid,metricname,companyinfo,df,dl,kl,model_type){
  
  csv_vars$quarterlabel <- NA
  csv_vars$End_date <- create_quarter_end(csv_vars$Date)
  csv_vars$quarter <- NA
  
  if (!is.na(csv_vars$Metric[nrow(csv_vars)-1]) & is.na(csv_vars$Metric[nrow(csv_vars)])){
    
    modeloutput <- model_spec$intcoeff.yoy + model_spec$xcoeff.yoy*csv_vars$YOY.ind[nrow(csv_vars)]

    if (as.character(model_spec$transform) == "yoyper"){ ### YOY - PREDICTION
      
      yoy_estimate <- modeloutput
      
    }else{ ### YOY-D PREDICTION
      
      yoy_estimate <- modeloutput/csv_vars[nrow(csv_vars)-4,]$Metric
      
    }
    
    level_estimate <- csv_vars$Metric[nrow(csv_vars)-4]*(1 + yoy_estimate)
    current_quarter_startdate <- csv_vars[nrow(csv_vars),]$Date
    quarter_startdate <- csv_vars[(nrow(csv_vars)-1),]$Date
    current_quarterlabel <- create_quarterlabel_list(quarterlist[quarterlist$Start_date == quarter_startdate,]$Quarter,2)[2]
    current_quarter_enddate <- csv_vars[nrow(csv_vars),]$End_date
   }else{
    
    modeloutput <- NaN
    level_estimate <- NaN
    yoy_estimate <- NaN
    current_quarter_startdate <- csv_vars[nrow(csv_vars),]$Date
    current_quarterlabel <- create_quarterlabel_list(quarterlist[quarterlist$Start_date == current_quarter_startdate,]$Quarter,2)[2]
    month(current_quarter_startdate) <- month(current_quarter_startdate) + 3
    current_quarter_enddate <- create_quarter_end(current_quarter_startdate) 
    newline <- data.frame(current_quarter_startdate,NaN,NaN,NaN,NaN,current_quarterlabel,
                          current_quarter_enddate,paste0(current_quarter_startdate," | ",current_quarterlabel))
    colnames(newline) <- colnames(csv_vars)
    csv_vars <- rbind(csv_vars,newline)
    
  }
  
  csv_vars$quarterlabel <- create_quarterlabel_list(current_quarterlabel,nrow(csv_vars),"backward")
  csv_vars$quarter <- paste(csv_vars$Date,"|",csv_vars$quarterlabel)
  vars_index = max(min(which(!is.na(csv_vars$YOY.ind))),min(which(!is.na(csv_vars$YOY.dep))))
  
  
  spec = list(
    "_id" = model_id,
    "current_quater" = list(
      "current_quarter_enddate" = current_quarter_enddate,
      "current_quarter_label" = current_quarterlabel,
      "model_output" = list("$numberDouble"= as.character(modeloutput)),
      "yoy_estimate" = list("$numberDouble" = as.character(yoy_estimate)),
      "level_estimate" = list("$numberDouble" = as.character(level_estimate))
    ),
    "linear_model" = list(
      "r^2_yoy" = model_spec$rsq.yoy,
      "intercept_yoy" = model_spec$intcoeff.yoy,
      "gradient_yoy" = model_spec$xcoeff.yoy,
      "r^2_level" = model_spec$rsq.level,
      "intercept_level" = model_spec$intcoeff.level,
      "gradient_level" = model_spec$xcoeff.level,
      "yoy_datapoints" = model_spec$numpts.yoy,
      "level_volatility" = list("$numberDouble" = NaN)
    ),
    "traffic_agg" = list(
      "traffic_agg" = handlingmongonan(csv_vars$Traffic,"NA"),
      "quarter" = csv_vars$quarter
    ),
    "reported_agg" = list(
      "reported_agg" = handlingmongonan(csv_vars$Metric),
      "quarter" = csv_vars$quarter
    ),
    "meta" = list(
      "metric" = metricname,
      "date" = as.character(as.numeric(Sys.time())*1000),
      "clusters" = array(),
      "platform" = platform_client_db_versions(model_spec$platform),
      "model_type" = model_type,
      "traffic_type" = form_client_db_versions(model_spec$form),
      "domain" = maindomain,
      "level_transformation" = level_transform_client_db_versions(model_spec$leveltransform),
      "yoy_transformation" = yoy_transform_client_db_versions(model_spec$transform),
      "list_of_domains" = dl,
      "list_of_keywords" = kl,
      "metric_id" = metricid,
      "company_id" = companyinfo$id
    ),
    "traffic_agg_mon" = list(
      "traffic_agg" = handlingmongonan(df$page_activity,"NA"),
      "month" = df$month
    ),
    "sample" = array(),
    "reported_yoy" = list(
      "reported_yoy" = handlingmongonan(csv_vars[vars_index:nrow(csv_vars),]$YOY.dep),
      "quarter" =  csv_vars[vars_index:nrow(csv_vars),]$quarter
    ),
    "traffic_yoy" = list(
      "traffic_yoy" = handlingmongonan(csv_vars[vars_index:nrow(csv_vars),]$YOY.ind),
      "quarter" =  csv_vars[vars_index:nrow(csv_vars),]$quarter
    )
  )
  spec = toJSON(spec, pretty = TRUE, auto_unbox = TRUE)
  return(spec)
}

handlingmongonan <- function(t,type = "NaN"){
  c= c()
  if(type=="NaN"){
    for(i in t){
      if(is.na(i)){
        c = c(c,list(list("$numberDouble"= "NaN")))
      }else{
        c = c(c,i)}
    }
  }else{
    for(i in t){
      if(is.na(i)){
        c = c(c,list(list("$numberDouble"= "NA")))
      }else{
        c = c(c,i)}
    }
  }
  return(c)
}

################################################# Final_models Database Connections ###############################################

## CLEAN
get_model_currquar<-function(con){
  query<-paste0("select * from model_current_quarter;")
  model_currquar<-dbGetQuery(con,query);
  return(model_currquar)
}


get_models_tobacktest <- function(con){
  query = 'SELECT md.model_config_id,company_id,max(data_points) as dp,forward_tracked FROM model_description md 
          join model_specification ms on md.model_config_id = ms.model_config_id where forward_tracked = 1
  group by model_config_id having dp >=6 order by dp desc;'
  models_to_test <- dbGetQuery(model_con,query);
  return(models_to_test)
}

get_models_toestimate <- function(con,ced){
    query <- paste0("select a.model_config_id,for_client,company_id,metric_id from model_specification a join model_current_quarter b on a.model_config_id = b.model_config_id where current_end_date = '",ced,"'and forward_tracked = 1;")
    models_toupdate <- dbGetQuery(con,query);
    return(models_toupdate)
}

## CLEAN 
get_all_forward_tracked_tickers <- function(fund_con,model_con){
  query <- 'select distinct(company_id) from model_specification where forward_tracked = 1;'
  companyids <- dbGetQuery(model_con,query);
  
  query <- paste0("select id,Ticker from Company where id in ",str_replace(paste0(c(companyids),collapse = ","),"c","")," order by Ticker;")
  companyids <- dbGetQuery(fund_con,query);
  return(companyids)
}

get_model_tickers <- function(con){
  query <- 'select distinct Ticker, C.id as company_id,category,sum(forward_tracked) as `FT Models` from fundamental_models.model_specification ms
            join fundamentals.Company C on C.id = ms.company_id group by Ticker order by Ticker;'
  return(dbGetQuery(con,query))
}

## CLEAN
get_all_client_tracked_models <- function(model_con,fund_con){
  query <- 'select model_config_id,company_id,category from model_specification where for_client = 1;'
  model_specification_list <- dbGetQuery(model_con,query)
  query <- paste0('select id,Ticker,Name from Company where id in (',
                  paste0(unique(model_specification_list$company_id),collapse = ','),') order by Name;')
  ticker_list <- dbGetQuery(fund_con,query);
  model_specification <- left_join(ticker_list,model_specification_list,by = c("id" = "company_id"))
  return(model_specification)
}

get_all_forward_tracked_models <- function(con){
  query <- 'select model_config_id from model_specification where forward_tracked = 1;'
  model_specification_list <- dbGetQuery(con,query)
  return(model_specification_list)
}

## CLEAN
get_model_description <- function(con,model_config_id,ced){
  query = paste0("SELECT * FROM model_description where end_date = '",ced,"' and  model_config_id = '",model_config_id,"';")
  model_description<-dbGetQuery(con,query);
  return(model_description)
}

## CLEAN
get_model_datapoints <- function(con,model_config_id){
  query <- paste0("select data_points from model_current_quarter a 
  join model_description b on a.current_end_date = b.end_date and a.model_config_id=b.model_config_id
  where a.model_config_id = '",model_config_id,"';")
  datapoints <- dbGetQuery(con,query);
  return(datapoints[1,1])
}

## CLEAN
get_model_error_rate <- function(con,model_config_id){
  query <- paste0("Select a.model_config_id,quarter_label,end_date,model_output,yoy_estimate,level_estimate,yoy_reported,level_reported,forecast_error,client_historical FROM model_error_rate a
                    join model_current_quarter b on a.model_config_id = b.model_config_id and a.end_date = b.current_end_date
                  where a.model_config_id = '",model_config_id,"';")
  model_error_rate <- dbGetQuery(con,query);
  return(model_error_rate)
}

## CLEAN
get_model_domain_list <- function(con,model_config_id){
  query <- paste0('Select * from model_domain_list where model_config_id = "',model_config_id,'";')
  return(dbGetQuery(con,query))
}


## CLEAN
get_tables_for_reportcard <- function(con){
  query <- 'select b.model_config_id,b.end_date,b.quarter_label,rsq,data_points,forecast_error,client_historical from fundamental_models.model_error_rate a 
                  join fundamental_models.model_description b on a.model_config_id = b.model_config_id and a.end_date = b.end_date;'
  error_rate <- dbGetQuery(con,query);
  
  query <- 'select a.model_config_id,company_id,for_client,forward_tracked,category,current_quarter_label,current_end_date from 
    fundamental_models.model_specification a join fundamental_models.model_current_quarter b on a.model_config_id = b.model_config_id;'
  model_specs <- dbGetQuery(con,query)
  return(list(error_rate,model_specs))
}

## CLEAN
get_all_info_for_model <-function(con,model_config_id,companyid = 0){
  if(companyid==0){
    query <- paste0('select * from model_specification where model_config_id = "',model_config_id,'";')
    model_specification <- dbGetQuery(con,query)
    query <- paste0('select * from model_description where model_config_id = "',model_config_id,'";')
    model_description <- dbGetQuery(con,query)
    query <- paste0('select * from model_current_quarter where model_config_id = "',model_config_id,'";')
    model_current_quarter <- dbGetQuery(con,query)
    query <- paste0('select * from model_raw_data where model_config_id = "',model_config_id,'" order by model_config_id,start_date;')
    model_raw_data <- dbGetQuery(con,query)
    query <- paste0('select * from model_error_rate where model_config_id = "',model_config_id,'";')
    model_error_rate <- dbGetQuery(con,query)
    query <- paste0('select * from model_domain_list where model_config_id = "',model_config_id,'";')
    model_domain_list <- dbGetQuery(con,query)
    
  }else{
    query <- paste0('select * from model_specification where company_id = ',companyid,' and forward_tracked = 1;')
    model_specification <- dbGetQuery(con,query)
    model_config_ids <- paste0(model_specification$model_config_id,collapse = "','")
    query <- paste0("select * from model_description where model_config_id in ('",model_config_ids,"');")
    model_description <- dbGetQuery(con,query)
    query <- paste0("select * from model_current_quarter where model_config_id in ('",model_config_ids,"');")
    model_current_quarter <- dbGetQuery(con,query)
    query <- paste0("select * from model_raw_data where model_config_id in ('",model_config_ids,"') order by model_config_id,start_date;")
    model_raw_data <- dbGetQuery(con,query)
    query <- paste0("select * from model_error_rate where model_config_id in ('",model_config_ids,"');")
    model_error_rate <- dbGetQuery(con,query)
    query <- paste0("select * from model_domain_list where model_config_id in ('",model_config_ids,"');")
    model_domain_list <- dbGetQuery(con,query)
    
  }
  return(list(model_specification,model_description,model_current_quarter,model_raw_data,model_error_rate,model_domain_list))
}

## CLEAN
check_model_in_database <- function(con,model_config_id){
  query <- paste0('select model_config_id from model_specification where model_config_id = "',model_config_id,'";')
  model_specification <- dbGetQuery(con,query)
  return(model_specification)
}


############################################## Database vs Client names conversions #####################################################
## CLEAN
platform_client_db_versions <- function(platform){
  if(platform == "total"){
    return("Total")
  }else if(platform == "desktop"){
    return("Desktop")
  }else if(platform == "Desktop"){
    return("desktop")
  }else if(platform == "Total"){
    return("total")
  }else if(platform == "Mobile"){
    return("mobile")
  }else if(platform == "mobile"){
    return("Mobile")
  } 
} 

## CLEAN
form_client_db_versions <- function(form){
  if(form=="visits"){
    return("Visits")
  }else if (form == "Visits"){
    return("visits")
  }else if(form == "Page Views"){
    return("pageviews")
  }else if(form == "pageviews"){
    return("Page Views")
  }
}

## CLEAN
level_transform_client_db_versions <- function(leveltransform){
  if(leveltransform == "asitis"){
    return("NT")
  }else if (leveltransform == "rollmean"){
    return("4-QMA")
  }else if(leveltransform == "NT"){
    return("asitis")
  }else if(leveltransform == "4-QMA"){
    return("rollmean")
  }else if(leveltransform == "4-QMA-M"){
    return("asitis")
  }else if(leveltransform == "4-QMA-B"){
    return("rollmean")
  }
}

## CLEAN
yoy_transform_client_db_versions <- function(yoytransform){
  if (yoytransform == "yoyper"){
    return("YOY")
  }else if (yoytransform == "yoydiff"){
    return("YOY-D")
  }else if (yoytransform == "YOY"){
    return("yoyper")
  }else if (yoytransform == "YOY-D"){
    return("yoydiff")
  }
}

################################################# WRITING TO DATABASES ################################################################
## CLEAN
write_database <- function(pushdf,tablename,con){
  dbWriteTable(con, tablename, pushdf, row.names = F, append = T)
  return(paste0("Database updated: ",tablename))
}

## CLEAN
converting_mongo_to_sql_table <- function(md,model_config_id){
  
  if(is.na(md$meta$list_of_keywords)){
    domains <- md$meta$list_of_domains[[1]]
    keywordlist <- NA
  }else{
    domains <- md$meta$domain
    keywordlist <- md$meta$list_of_keywords[[1]]
  } 

  model_specification <- data.frame(model_config_id = model_config_id,company_id = md$meta$company_id,metric_id = md$meta$metric_id,
                                    for_client = NA,forward_tracked = NA,category = NA, logic = NA, not_forward_tracked_reason = NA)

  model_current_quarter <- data.frame(model_config_id = model_config_id, current_quarter_label = md$current_quater$current_quarter_label,current_end_date = as.Date(md$current_quater$current_quarter_enddate))
  
  model_description <- data.frame(model_config_id = model_config_id, quarter_label = md$current_quater$current_quarter_label,end_date = as.Date(md$current_quater$current_quarter_enddate),
                           data_points = md$linear_model$yoy_datapoints, rsq = md$linear_model$`r^2_yoy`, intercept = md$linear_model$intercept_yoy, gradient = md$linear_model$gradient_yoy)
  
  model_error_rate <- data.frame(model_config_id = model_config_id, quarter_label = md$current_quater$current_quarter_label,end_date = as.Date(md$current_quater$current_quarter_enddate),
                                 model_output = md$current_quater$model_output, yoy_estimate = md$current_quater$yoy_estimate, level_estimate = md$current_quater$level_estimate)
  
  
  raw_agg <- data.frame(md$traffic_agg$quarter, md$traffic_agg$traffic_agg,md$reported_agg$reported_agg)
  colnames(raw_agg) <- c("quarter","tomahawk_metric","company_metric")
  raw_yoy <- data.frame(md$traffic_yoy$quarter,md$traffic_yoy$traffic_yoy,md$reported_yoy$reported_yoy)
  colnames(raw_yoy) <- c("quarter","tomahawk_yoy","company_yoy")
  raw_data <- left_join(raw_agg,raw_yoy, on = "quarter")
  raw_index = min(which(!is.na(raw_data$tomahawk_yoy)))-4
  raw_data <- raw_data[raw_index:nrow(raw_data),]
  raw_data <- raw_data %>% separate(quarter,c("start_date","quarter_label")," \\| ")
  raw_data$end_date <- create_quarter_end(raw_data$start_date)
  raw_data$model_config_id <- model_config_id
  raw_data <- raw_data %>% select(model_config_id,start_date,end_date,quarter_label,tomahawk_metric,company_metric,tomahawk_yoy,company_yoy)
  
  domain_ids <- c()
  for (d in domains){
    domain_ids <- c(domain_ids,get_domain_id(traffic_con,d))
  }
  model_domain_list <- data.frame(model_config_id = model_config_id,domain_id = domain_ids,keyword = keywordlist,yoy_transformation = md$meta$yoy_transformation, 
             level_transformation = md$meta$level_transformation,platform = md$meta$platform,traffic_type = md$meta$traffic_type)
  
  return(list(model_specification,model_description,model_current_quarter,model_error_rate,raw_data,model_domain_list))
}