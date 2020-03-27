
################################### FORWARD TRACKING FUNDAMENTAL MODELS ###################################
 
# Libraries ---------------------------------------------------------------------------------------------
rm(list= ls())

library(pool)
library(ggplot2)
library(data.table)
library(dplyr)
library(shiny)
library(jsonlite)
library(stringr)
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
library(R.utils)
options(warn=-1)

# Setting Directory --------------------------------------------------------------------------------------

source('../serverfunc.R')
source('../config.R')

lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
app_keys <- fromJSON(db_file)$app_fmodels_read
model_keys <- fromJSON(db_file)$prod_fundamental_models_write
traffic_con <- dbPool(drv = RMySQL::MySQL(), host = app_keys$host,dbname = app_keys$dbname[3], username = app_keys$id, password = app_keys$pw)
fund_con <- dbPool(drv = RMySQL::MySQL(), host = app_keys$host, dbname = app_keys$dbname[2], username = app_keys$id, password = app_keys$pw)
model_con <- dbPool(drv = RMySQL::MySQL(), host = model_keys$host,dbname = model_keys$dbname , username = model_keys$id, password = model_keys$pw)
companyids <- get_all_forward_tracked_tickers(fund_con,model_con)


# Shiny JS  ----------------------------------------------------------------------------------------------------------
jscode <- "shinyjs.toTop = function() { document.body.scrollTop = 0; document.documentElement.scrollTop = 0;}"
