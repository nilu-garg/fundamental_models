# Generator

 ### Generating new models for a given ticker

## Initial Steps -
 ### For a new ticker in the pipeline -
	 - Identify domains that can have a relation with the earnings data
	 - Check if the fundamentals database is uptodate with the reported metrics for the ticker
         - Add the domains to the appropriate category backtest group using the strat too. eg "Consumer backtest/Utilities backtest"
	 - Run the traffic tool to add visits and ppv for these domains to the database

## Individual Tabs - 

 ### Review strat database
	 - user can enter a domain to see the database has the relevant traffic/pp data
	 - outputs a table with the domain, platform, min and max date in the database
 ### Explore Popular Pages
	 - user can explore subdomains and keyword frequencies for the entered domain
	 - after the user enters the domain, three tables are displayed
	 - first table lists the subdomains and their frequency
         - second table lists the tokens and their frequency
         - third table displays the raw popular pages data
	 - enter the domain in the databaseyou can check if the traffic database has the data for a particular domain
 ### Create Models 
         - runs two loops(subdomain and keyword) to create models for the entered ticker
         - subdomain loop - user enters the ticker and list of domains separated by ';'
	 - keyword loop - user enters ticker, domains and list of keywords separated by ';'
         - the app outputs a table after the loop ends listing the filtered models
 ### Visualize Models
         - outputs the regression and line graphs for the a specific model
         - user enters a ticker and the model_config_id and the app outputs four graphs
         - if the user wants to push the given model in the sql database, he needs to add the model specfication data 
           after which the models is added to the dataabse for forward tracking