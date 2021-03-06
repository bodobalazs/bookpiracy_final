#DATA SETUP - prepare all data
library(tidyverse)

# Load EUROSTAT variables. Contains the followind frames:
# bookpiracy - all EUROSTAT data documented in 
# paper/bookpiracy_final_description_2020_06_07.pdf
# https://figshare.com/account/projects/80837/articles/12443474
# 10.21942/uva.12443474
# bookpiracy_var_stat - the metadata of the Eurostat variables 
# 10.21942/uva.12443468 [https://figshare.com/account/projects/80837/articles/12443468]
# The long format version of the data file is 
# 10.21942/uva.12443465 [https://figshare.com/account/projects/80837/articles/12443465]
# bookpiracy_wide - genereated here from the long-format with metadata
# loss, the long format version has more metadata, the wide is used for 
# further computations.
# The data from Eurobarometer is described 
# https://figshare.com/account/projects/80837/articles/12389384
# https://doi.org/10.21942/uva.12389384

load (file.path('data-raw', 'bookpiracy_2020-04-15.rda'))

# load the Eurobarometer variable
# The data is available and described in on FIGSHARE (UPDATE WHEN FINAL)
# at https://zenodo.org/record/3759811#.XqKbj2gzbIU
# a longer description of the methodology can be found 
# https://figshare.com/articles/Regionalized_Cultural_Access_and_Participation_Books_And_Libraries_And_Science_Attitudes_Variables/12170190

eurobarometer <- read.csv (
  file.path('data-raw', 
            'books_library_eurobarometer_79_2.csv'), 
  stringsAsFactors = FALSE) 

## The Eurobarometer survey fieldwork took place in April-May
## 2013 and most the questions relate to the previous 12 months.

eb_vars <- eurobarometer %>%
  dplyr::select ( geo, indicator, values, method ) %>%
  dplyr::mutate ( indicator = gsub("erobaro", "eurobaro", indicator)) %>%
  dplyr::mutate ( indicator = gsub("eurobarometer_79_2", "eb", indicator))

# merge the data ---------------------------------------------------
# add number of researchers per region 
researchers <- bookpiracy %>%
  dplyr::select ( geo, indicator, values, method ) %>%
  filter ( indicator == 'rd_p_persreg_total_t_total_fte' ) %>%
  dplyr::select ( geo, values, method ) %>%
  mutate ( indicator  = 'researchers_total' )

# add percentage of researchers
bookpiracy_long <- bookpiracy %>%
  filter ( ! grepl("rd_p_persreg", indicator ))%>%
  dplyr::select ( geo, values, indicator, method )%>%
  rbind ( researchers) 

# The geodata is a administrative boundary map of the NUTS2 regions in the
# NUTS2016 definitions, as provided by Eurostat and Eurographics.
# We translated the shapefile to an sf object for easier handling. 

geodata <- readRDS ( 'data-raw/geodata.rds' )

# add download counts 
# the data was prepared by count_over_nuts_2.R

download_data <- readRDS( file.path("data-raw","downloads_nuts_2016.rds")
) %>%
  dplyr::rename ( geo = id, 
                  values = count ) %>%
  dplyr::mutate ( indicator = 'count',
                  method = 'actual' ) %>%
  filter ( geo %in% bookpiracy_long$geo ) %>%
  mutate ( values = ifelse(is.na(values), 0, values))

# rename variables to meaningful names -------------------------
indicator_renaming <- read.csv("data-raw/indicator_renaming.csv", 
                               stringsAsFactors = FALSE)

unique(bookpiracy_long$indicator) # var names BEFORE renaming

# nuts2dataset is the base dataframe for the analysis n long format 
# it contains incomplete cases

nuts2_dataset <- bookpiracy_long %>%
  left_join (  indicator_renaming, by = "indicator")  %>%
  mutate ( indicator = ifelse(is.na(new_name), 
                              indicator, new_name) ) %>%
  mutate ( indicator = gsub("erobarometer_79_2_|eurobarometer_79_2_", "", indicator)) %>%
  dplyr::select (-new_name ) %>%
  rbind(download_data ) 

unique(nuts2_dataset$indicator) # var names AFTER renaming

# create data frame with different relative dowload counts 

eurostat_data  <- nuts2_dataset %>%
  dplyr::select ( -method ) %>% 
  tidyr::spread ( indicator, values ) %>%
  dplyr::mutate ( count_per_million = round(1000000*count / population_total),
                  count_per_capita = count / population_total, 
                  count_per_area   = count / area_land_filled,
                  count_per_thousand_researchers = round(1000*count / researchers_total),
                  count_per_researcher  = count / researchers_total) %>%
  dplyr::mutate ( count_per_pop_density = {count /
      ( population_total / area_land_filled)} ,
      count_per_researcher = {
        count / researchers_total } ) 

#complete_df is the base dataset for analysis. it only contains complete cases
eurostat_complete_data <- eurostat_data %>% 
  drop_na()

eurostat_eurobarometer_data <- nuts2_dataset %>%
  dplyr::select ( -all_of("method") ) %>%
  bind_rows ( eb_vars %>%
                dplyr::select  (-all_of("method") )) %>%
  tidyr::spread ( indicator, values ) %>%
  dplyr::mutate ( count_per_million = round(1000000*count / population_total),
                  count_per_capita = count / population_total, 
                  count_per_area   = count / area_land_filled,
                  count_per_thousand_researchers = round(1000*count / researchers_total),
                  count_per_researcher  = count / researchers_total) %>%
  dplyr::mutate ( count_per_pop_density = {count /
      ( population_total / area_land_filled)} ,
      count_per_researcher = {
        count / researchers_total } ) 

eurostat_eurobarometer_complete_data <- eurostat_eurobarometer_data %>%
  drop_na()

names ( eurostat_eurobarometer_complete_data )

#create datasets with scaled varibles
eurostat_scaled <- dplyr::mutate_at(.tbl = eurostat_data, 
                                    .vars = dplyr::vars(-starts_with("count"), 
                                                        -all_of("geo")), 
                                    .funs = scale )  

eurostat_eurobarometer_scaled <- eurostat_eurobarometer_data  %>%
  dplyr::mutate_at( dplyr::vars(-starts_with("count"),
                                -all_of(c("geo"))), scale )   

eurostat_complete_scaled <- eurostat_complete_data %>%
  dplyr::mutate_at( dplyr::vars(-starts_with("count"), 
                                -all_of(c("geo"))), scale )

eurostat_eurobarometer_complete_scaled <- eurostat_eurobarometer_complete_data %>%
  dplyr::mutate_at( dplyr::vars(-starts_with("count"), 
                                -all_of(c("geo"))), scale )


dataset_source_statistics <- nuts2_dataset %>%
  dplyr::group_by( method, indicator ) %>%
  dplyr::add_count ( ) %>%
  distinct ( indicator, method, n  ) %>%
  spread ( method, n )

save(eurostat_data, 
     eurostat_complete_data, 
     eurostat_complete_scaled,
     eurostat_eurobarometer_data,
     eurostat_eurobarometer_scaled,
     eurostat_eurobarometer_complete_data,
     eurostat_eurobarometer_complete_scaled,
     dataset_source_statistics, 
     geodata,
     file=file.path("data", "all_data_for_analysis.rda"))
