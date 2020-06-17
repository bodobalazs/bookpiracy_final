This repo will ultimately be turned into a public one to be sent out for reviewers.

## R analyses
The european and global models are contained in separate files in the root folder:
- global_models_20200525.R: contains the global models
- EU_NUTS2_analysis.Rmd: contains the EU models

## Paper

The manuscript of the paper can be found in the `/paper` folder.

## data-raw

The `data-raw` folder contains all raw data and pre-processed data for analyis.
- `bookpiracy_2020-04-15.rda`: contains all the variables obtained and processed from Eurostat
- `books_library_eurobarometer_79_2.csv`: EUROBAROMETER library usage survey data
- all_linreg_coefficients.rds: 
- country_download.csv: global dowload per country dataset
- downloads_nuts_2016.rds; EU NUTS2 download per region dataset
- geodata.rds: NUTS2 regional geodata
- geodata_sf.rda: NUTS2 regional geodata
- gov_spending_edu.csv: UNESCO government spending on education per country data for 2015
- indicator_renaming.csv: technical files to rename EUROBAROMETER and  EUROSTAT variables
- public_spending_education_oecd.csv: OECD public spending on tertiaty education 
- scimagojr_2015.xlsx: SCIMag H index list
- worldbank_data_20200525.rda: al worldbank data tables: population, GDP, internet penetration, R&D activity, literacy
- worldbank_income_category.csv: worldbank income categories

## Data  

The folder `data` contains all the data for the analysis. In this folder, the 

- global_data_20200525.rda: all merged data for global analysis.

- `all_data_for_analysis.rda` contains several versions of our data for the European analysis:

	- `geodata`: the download count data aggregated over the boundaries of the NUTS2 regions (following the NUTS2016 definition)
	- `eurostat_data`: contains all Eurostat variables from `bookpiracy_2020-04-15.rda`  and the count data from `geodata` 
	- `eurostat_complete_data`: the filteres version of the `eurostat_data`, only regions where all variables are available `eurostat_complete_data`
	- `eurostat_complete_scaled`: the scaled version of the 
	- `eurostat_eurobarometer_data`: all Eurostat variables and variables created from Eurobarometer
	- `eurostat_eurobarometer_complete_data`: the previous data set filtered for the regions where all data is available - this dataset is smaller than the one with Eurostat-only variables
	- `eurostat_eurobarometer_scaled`: the scaled version of the `eurostat_eurobarometer_complete_data`
	- `dataset_source_statistics` is an overview of the variables.
