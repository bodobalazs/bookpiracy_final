This repo will ultimately be turned into a public one to be sent out for reviewers.

## Paper

The manuscript of the paper can be found in the `/paper` folder.

## data-raw

The `data-raw` folder contains all raw data and pre-processed data for analyis.
- `bookpiracy_2020-04-15.rda`: contains all the variables obtained and processed from Eurostat
- `books_library_eurobarometer_79_2.csv`

## Data  

The folder `data` contains all the data for the analysis. In this folder, the `all_data_for_analysis.rda` contains several versions of our data:

- `geodata`: the download count data aggregated over the boundaries of the NUTS2 regions (following the NUTS2016 definition)
- `eurostat_data`: contains all Eurostat variables from `bookpiracy_2020-04-15.rda`  and the count data from `geodata` 
- `eurostat_complete_data`: the filteres version of the `eurostat_data`, only regions where all variables are available `eurostat_complete_data`
- `eurostat_complete_scaled`: the scaled version of the 
- `eurostat_eurobarometer_data`: all Eurostat variables and variables created from Eurobarometer
- `eurostat_eurobarometer_complete_data`: the previous data set filtered for the regions where all data is available - this dataset is smaller than the one with Eurostat-only variables
- `eurostat_eurobarometer_scaled`: the scaled version of the `eurostat_eurobarometer_complete_data`
- `dataset_source_statistics` is an overview of the variables.

