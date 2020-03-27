############################################ FUNDAMENTALS DASHBOARD ###################################################

# Libraries ---------------------------------------------------------------------------------------------
rm(list= ls())

jscode <- "shinyjs.toTop = function() { document.body.scrollTop = 0; document.documentElement.scrollTop = 0;}"

library(pool)
library(ggplot2)
library(data.table)
library(dplyr)
library(shiny)
library(jsonlite)
library(RMySQL)
library(stringr)
library(tidyr)
library(urltools)
library(lubridate)
library(shinythemes)
library(openxlsx)
library(shinyMatrix)
library(shinyjs)
options(warn=-1)

source('../serverfunc.R')
source('../config.R')
keys<-fromJSON(db_file)$prod_fundamentals_write
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
con <- dbPool(drv = RMySQL::MySQL(), host = keys$host, dbname = keys$dbname, username = keys$id, password = keys$pw)

existingcompanies <- sort(get_all_company_list(con)$Name)
dbmetricslist <- get_metric_list(con,1)
metricslist <- sort(dbmetricslist$Metric)



