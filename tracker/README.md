# Tracker

 ### Forward tracking live traffic models

## Initial Steps - 
 ### When similar web adds in a new month -
	- In the traffic_tool, for the project saas_backtest run the tool for the new month for groups - SaaS Backtest and Consumer Backtest
	- In the fundamental_models database - "run the query - select domainlist from model_specs where forward_tracked = 1 and keywordlist is not null;"
	- For the domains in the result, get the popular pages and update the strat_new database using strat_tool for the just the new month of popular pages

 ### When company reports -
	- Linell/Ryan update the fundamentals database using the fundamentals_dash

## Individual Tabs -

 ### Checking if Database is Updated (For new similarweb data)
	- Output - A list of domains that are not updated for the new month in the strat_new database
	- When everything is in the database, it outputs a message "Everything is updated for forwardtracking"

 ### Estimating Values for New Quarter (For new similarweb data)
	- Input - End of the most recent quarter
	- Output - A list of models that give an estimate for the entered quarter.
	- Download Data - The zip has three files - 
		- excel_updates - has the ticker and new estimates for the client models
		- error_rate - push the data to model_error_rate in fundamentals_models
		- raw_data - update the rows with the new data in model_raw_data in fundamentels_models

 ### Getting Monthly Data for Client Models (For new similarweb data)
	- Input - Company Category
	- Output - A dataframe with the monthly data for all the client models
	- Download Data - A csv with the monthly data for the client models of the selected category 	

 ### Updating Individual Ticker Models (For new reported data)
	- Input - Ticker
	- Output - A list of models, their most recent rsq, forecast_error, and model_status
