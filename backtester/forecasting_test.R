install.packages("rstan")
install.packages("prophet",type = "source")

library(prophet)

setwd('C:/Users/ngarg/Documents/Projects/fundamental_models/backtester/')
source('../config.R')

domain = 'aep.com'

lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
app_keys <- fromJSON(db_file)$app_fmodels_read
tillotechdb_cons <- dbPool(drv = RMySQL::MySQL(),host = app_keys$host, username = app_keys$id, password = app_keys$pw)
query = paste0("select tp.date,pages_per_visit,visits from strat_new.traffic_ppv tp ",
               "join strat_new.traffic_visits tv on ",
               "tv.domain_id = tp.domain_id and tv.platform = tp.platform and tv.date = tp.date and tv.granularity = tp.granularity ",
               "join strat_new.domains d on tv.domain_id = d.domain_id ",
               "where domain = '",domain,"' and tp.platform = 'desktop' and tp.granularity = 'Daily' ;")

daily_data<-dbGetQuery(tillotechdb_cons,query)
daily_data$pageviews <- daily_data$pages_per_visit * daily_data$visits
df <- daily_data %>% select("date","pageviews")
plot(df)


df$date = as.Date(df$date)
colnames(df) <- c("ds","y")

m <- prophet(df,changepoint.prior.scale = 0.5)

## Model Initialization
m <- prophet(changepoint.prior.scale = 0.5)
m <- add_country_holidays(m,country_name = 'US')
m <- fit.prophet(m,df)

### Deciding on seasonality smoothness to fit changing cycles better by changing fourier series terms 
m <- prophet(df,yearly.seasonality = 30)
prophet:::plot_yearly(m)


m <- prophet(yearly.seasonality = 15, changepoint.prior.scale = 0.5,weekly.seasonality = 15)
m <- add_country_holidays(m,country_name = 'US')
m <- add_seasonality(m,name = 'quarterly',period = 91.25,fourier.order = 5)
m <- fit.prophet(m,df)
prophet:::plot_yearly(m)
prophet:::plot_weekly(m)
future <- make_future_dataframe(m, periods = 365)#, freq = 'week',include_history = F)
forecast <- predict(m,future)
plot(m,forecast)
plot(m,forecast) + add_changepoints_to_plot(m)
prophet_plot_components(m, forecast)
