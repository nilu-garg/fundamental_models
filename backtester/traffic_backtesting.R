#library(caret)

rm(list= ls())

# Libraries ---------------------------------------------------------------------------------------------

library(pool)
library(ggplot2)
library(data.table)
library(dplyr)
library(shiny)
library(jsonlite)
library(stringr)
library(googlesheets)
library(urltools)
library(lubridate)
library(openxlsx)
library(DT)
library(zoo)
library(shinythemes)
library(RMySQL)
library(forecast)
library(tidyr)
library(mongolite)
library(zip)
options(warn=-1)

# Setting Directory --------------------------------------------------------------------------------------

#setwd('C:/Users/ngarg/Documents/Projects/fundamental_models/backtester')
source('../serverfunc.R')
source('../config.R')

lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

app_keys <- fromJSON(db_file)$app_fmodels_read
traffic_con <- dbPool(drv = RMySQL::MySQL(), host = app_keys$host,dbname = app_keys$dbname[3], username = app_keys$id, password = app_keys$pw)
fund_con <- dbPool(drv = RMySQL::MySQL(), host = app_keys$host, dbname = app_keys$dbname[2], username = app_keys$id, password = app_keys$pw)
model_con <- dbPool(drv = RMySQL::MySQL(), host = app_keys$host,dbname = app_keys$dbname[1], username = app_keys$id, password = app_keys$pw)

# Getting the models from the database --------------------------------------------------------------------

error_table_main = data.frame()
error_comparison_main <- data.frame()

models_to_test <- get_models_tobacktest(model_con)
for (model_config_id in models_to_test$model_config_id){
  
  print(model_config_id)
  res <- get_all_info_for_model(model_con,model_config_id)
  model_specification <- res[[1]]
  model_raw_data <- res[[4]]
  model_domain_list <- res[[6]]
  
  yoytransform <- yoy_transform_client_db_versions(unique(model_domain_list$yoy_transformation))
  vars <- model_raw_data %>% select(end_date,tomahawk_yoy,company_yoy) %>% filter(!is.na(company_yoy))
  total_data_points <- nrow(vars)
  training_error_table = data.frame()
  test_error_table = data.frame()

  for (window in 4:(total_data_points-1)){
    for(m in window:(total_data_points-1)){
        model_inquestion <- vars[(m-window+1):m,]
        test_inquestion <- vars[(m+1):nrow(vars),]
        regyoy <- summary(lm(company_yoy ~ tomahawk_yoy,model_inquestion ))
        rsq.yoy<- regyoy$r.squared
        intcoeff.yoy <- regyoy$coefficients[1,c("Estimate")]
        xcoeff.yoy <- regyoy$coefficients[2,c("Estimate")]
    
        #### Training Error Loop - 
        for (t in 1:nrow(model_inquestion)){
      
          end_date_predictor <- model_inquestion$end_date[t]
          end_date_prev_quar <- as.Date(end_date_predictor)
          year(end_date_prev_quar) <- year(end_date_prev_quar)-1
          end_date_prev_quar <- as.character(end_date_prev_quar)
          model_input <- model_raw_data$tomahawk_yoy[which(model_raw_data$end_date==end_date_predictor)]
          prev_quar_level <- model_raw_data$company_metric[which(model_raw_data$end_date==end_date_prev_quar)]
          estimate_result <- cal_model_estimate(model_input,intcoeff.yoy,xcoeff.yoy,yoytransform,prev_quar_level)
          model_output <- estimate_result[[1]]
          estimate_yoy <- estimate_result[[2]]
          estimate_level <- estimate_result[[3]]
          if(yoytransform=="yoydiff"){
            reported_yoy <- model_raw_data$company_yoy[which(model_raw_data$end_date==end_date_predictor)]/model_raw_data$company_metric[model_raw_data$end_date==end_date_prev_quar]
          }else{
            reported_yoy <- model_raw_data$company_yoy[which(model_raw_data$end_date==end_date_predictor)]
          }
          reported_level <- model_raw_data$company_metric[which(model_raw_data$end_date==end_date_predictor)]
          residual_yoy <- estimate_yoy - reported_yoy
          residual_level <- estimate_level - reported_level
          residual_yoy_square <- residual_yoy^2
          residual_level_square <- residual_level^2
          avg_training_yoy <- mean(model_raw_data$company_yoy[model_raw_data$end_date %in% model_inquestion$end_date],na.rm = T)
          avg_training_level <- mean(model_raw_data$company_metric[model_raw_data$end_date %in% model_inquestion$end_date],na.rm = T)
          avg_residual_yoy <- avg_training_yoy - reported_yoy
          avg_residual_level <- avg_training_level - reported_level
          avg_residual_yoy_square <- avg_residual_yoy^2
          avg_residual_level_square <-  avg_residual_level^2
          ter <- data.frame(model_config_id = model_config_id,end_date_training = model_inquestion$end_date[nrow(model_inquestion)],
                        training_datapoints = nrow(model_inquestion), rsq = rsq.yoy, intcoeff = intcoeff.yoy,xcoeff = xcoeff.yoy,
                        end_date_predictor = end_date_predictor,end_date_prev_quar = end_date_prev_quar,
                        model_input = model_input,model_output = model_output,estimate_yoy = estimate_yoy,estimate_level = estimate_level,
                        reproted_yoy = reported_yoy,reported_level = reported_level, residual_yoy = residual_yoy,residual_level = residual_level,
                        residual_yoy_square = residual_yoy_square,residual_level_square = residual_level_square,
                        avg_training_yoy = avg_training_yoy,avg_training_level = avg_training_level,
                        avg_residual_yoy = avg_residual_yoy,avg_residual_level = avg_residual_level,
                        avg_residual_yoy_square = avg_residual_yoy_square,avg_residual_level_square = avg_residual_level_square)
          training_error_table <- rbind(training_error_table,ter)
      
        }
    
    #### Test Error Loop - 
        for (t in 1:nrow(test_inquestion)){
      
          end_date_predictor <- test_inquestion$end_date[t]
          end_date_prev_quar <- as.Date(end_date_predictor)
          year(end_date_prev_quar) <- year(end_date_prev_quar)-1
          end_date_prev_quar <- as.character(end_date_prev_quar)
          model_input <- model_raw_data$tomahawk_yoy[which(model_raw_data$end_date==end_date_predictor)]
          prev_quar_level <- model_raw_data$company_metric[which(model_raw_data$end_date==end_date_prev_quar)]
          estimate_result <- cal_model_estimate(model_input,intcoeff.yoy,xcoeff.yoy,yoytransform,prev_quar_level)
          model_output <- estimate_result[[1]]
          estimate_yoy <- estimate_result[[2]]
          estimate_level <- estimate_result[[3]]
          if(yoytransform=="yoydiff"){
            reported_yoy <- model_raw_data$company_yoy[which(model_raw_data$end_date==end_date_predictor)]/model_raw_data$company_metric[model_raw_data$end_date==end_date_prev_quar]
          }else{
            reported_yoy <- model_raw_data$company_yoy[which(model_raw_data$end_date==end_date_predictor)]
          }
          reported_level <- model_raw_data$company_metric[which(model_raw_data$end_date==end_date_predictor)]
          residual_yoy <- estimate_yoy - reported_yoy
          residual_level <- estimate_level - reported_level
          residual_yoy_square <- residual_yoy^2
          residual_level_square <- residual_level^2
          avg_training_yoy <- mean(model_raw_data$company_yoy[model_raw_data$end_date %in% model_inquestion$end_date],na.rm = T)
          avg_training_level <- mean(model_raw_data$company_metric[model_raw_data$end_date %in% model_inquestion$end_date],na.rm = T)
          avg_residual_yoy <- avg_training_yoy - reported_yoy
          avg_residual_level <- avg_training_level - reported_level
          avg_residual_yoy_square <- avg_residual_yoy^2
          avg_residual_level_square <-  avg_residual_level^2
          ter <- data.frame(model_config_id = model_config_id,end_date_training = model_inquestion$end_date[nrow(model_inquestion)],
                        training_datapoints = nrow(model_inquestion), rsq = rsq.yoy, intcoeff = intcoeff.yoy,xcoeff = xcoeff.yoy,
                        end_date_predictor = end_date_predictor,end_date_prev_quar = end_date_prev_quar,
                        model_input = model_input,model_output = model_output,estimate_yoy = estimate_yoy,estimate_level = estimate_level,
                        reproted_yoy = reported_yoy,reported_level = reported_level, residual_yoy = residual_yoy,residual_level = residual_level,
                        residual_yoy_square = residual_yoy_square,residual_level_square = residual_level_square,
                        avg_training_yoy = avg_training_yoy,avg_training_level = avg_training_level,
                        avg_residual_yoy = avg_residual_yoy,avg_residual_level = avg_residual_level,
                        avg_residual_yoy_square = avg_residual_yoy_square,avg_residual_level_square = avg_residual_level_square)
          test_error_table <- rbind(test_error_table,ter)
      
    }
    
    }
  }

  training_error_table$forecast_error <- training_error_table$residual_level/training_error_table$reported_level
  training_error_table$abs_forecast_error <- abs(training_error_table$forecast_error)
  avg_training_abs_forecast_error <- mean(training_error_table$abs_forecast_error,na.rm = T)
  stddev_training_abs_forecast_error <- sd(training_error_table$abs_forecast_error,na.rm = T)
  RMSE_training_level <- sqrt(sum(training_error_table$residual_level_square)/nrow(training_error_table))
  RMSE_training_yoy <- sqrt(sum(training_error_table$residual_yoy_square)/nrow(training_error_table))
  RRSE_training_level <- sqrt(sum(training_error_table$residual_level_square)/sum(training_error_table$avg_residual_level_square))
  RRSE_training_yoy <- sqrt(sum(training_error_table$residual_yoy_square)/sum(training_error_table$avg_residual_yoy_square))


  test_error_table$forecast_error <- test_error_table$residual_level/test_error_table$reported_level
  test_error_table$abs_forecast_error <- abs(test_error_table$forecast_error)
  avg_test_abs_forecast_error <- mean(test_error_table$abs_forecast_error,na.rm = T)
  stdev_test_abs_forecast_error <- sd(test_error_table$abs_forecast_error,na.rm = T)
  RMSE_test_level <- sqrt(sum(test_error_table$residual_level_square)/nrow(test_error_table))
  RMSE_test_yoy <- sqrt(sum(test_error_table$residual_yoy_square)/nrow(test_error_table))
  RRSE_test_level <- sqrt(sum(test_error_table$residual_level_square)/sum(test_error_table$avg_residual_level_square))
  RRSE_test_yoy <- sqrt(sum(test_error_table$residual_yoy_square)/sum(test_error_table$avg_residual_yoy_square))

  error_comparison_table <- data.frame(model_config_id = model_config_id,avg_training_abs_forecast_error = avg_training_abs_forecast_error,stddev_training_abs_forecast_error = stddev_training_abs_forecast_error,
           RMSE_training_level = RMSE_training_level,RMSE_training_yoy = RMSE_training_yoy,RRSE_training_level = RRSE_training_level,RRSE_training_yoy = RRSE_training_yoy,
           avg_test_abs_forecast_error = avg_test_abs_forecast_error,stdev_test_abs_forecast_error =stdev_test_abs_forecast_error,
           RMSE_test_level = RMSE_test_level,RMSE_test_yoy = RMSE_test_yoy,RRSE_test_level = RRSE_test_level,RRSE_test_yoy = RRSE_test_yoy)


  training_error_table$type = "Train"
  test_error_table$type = "Test"
  error_table_main = rbind(error_table_main,training_error_table)
  error_table_main = rbind(error_table_main,test_error_table)
  error_comparison_main <- rbind(error_comparison_main,error_comparison_table)
}


wb = createWorkbook()
addWorksheet(wb,"error_comparison")
addWorksheet(wb,"train_test_error")
writeData(wb,"error_comparison",error_comparison_main,startRow = 1,rowNames = F)
writeData(wb,"train_test_error",error_table_main,startRow = 1,rowNames = F)
saveWorkbook(wb,paste0("backtesting_models - ",as.character(Sys.Date()),".xlsx"),overwrite = T)