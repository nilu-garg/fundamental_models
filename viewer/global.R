################################### FUNDAMENTAL MODELS VIEWING ###################################

# Libraries ---------------------------------------------------------------------------------------------
rm(list= ls())

library(pool)
library(ggplot2)
library(data.table)
library(dplyr)
library(shiny)
library(jsonlite)
library(stringr)
library(lubridate)
library(openxlsx)
library(DT)
library(zoo)
library(shinythemes)
library(RMySQL)
library(tidyr)
library(zip)
library(R.utils)
options(warn=-1)

# Setting Directory --------------------------------------------------------------------------------------

source('../serverfunc.R')
source('../config.R')

lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
app_keys <- fromJSON(db_file)$app_fmodels_read
tillotechdb_cons <- dbPool(drv = RMySQL::MySQL(),host = app_keys$host, username = app_keys$id, password = app_keys$pw)
#fund_con <- dbPool(drv = RMySQL::MySQL(), host = app_keys$host, dbname = app_keys$dbname[2], username = app_keys$id, password = app_keys$pw)
#model_con <- dbPool(drv = RMySQL::MySQL(), host = app_keys$host,dbname = app_keys$dbname[1], username = app_keys$id, password = app_keys$pw)
#companytickers <- get_all_forward_tracked_tickers(fund_con,model_con)
model_tickers <- get_model_tickers(tillotechdb_cons)
model_tickers$Ticker_Name <- paste0(model_tickers$Ticker," : ",model_tickers$category)
categories_name <- c('SaaS','Consumer','Healthcare','Financial','Industrial','Telecom','Utilities')

# Shiny JS  ----------------------------------------------------------------------------------------------------------
jscode <- "shinyjs.toTop = function() { document.body.scrollTop = 0; document.documentElement.scrollTop = 0;}"
