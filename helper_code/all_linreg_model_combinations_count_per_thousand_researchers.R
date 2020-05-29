library(tidyverse)
library(broom)
## loading file created by helper_code/prepare_data.R
load (file = file.path("data", "all_data_for_analysis.rda"))

## load auxilliary functions for looping
source(file.path("helper_code", "aux_functions_for_looping.R"))

## Run the models
run_researcher_models <- function(
                   params, 
                   target_variable = 'count_per_thousand_researchers', 
                   model_df = researchers_dataset_scaled ) {
  x1 = unlist(params)[1]
  x2 = unlist(params)[2]
  
  vars_to_select <- as.character(
    c("geo", target_variable, x1,x2)
  )
  
  model_data <- model_df  %>% 
    dplyr::select ( tidyselect::all_of(vars_to_select) ) %>%
    tidyr::pivot_longer ( cols = -one_of('geo')) %>%
    dplyr::filter ( complete.cases(.)) %>%
    tidyr::pivot_wider ( names_from = 'name', values_from = 'value') %>%
    dplyr::filter ( complete.cases(.))
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( target_variable, ' ~ ', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( target_variable, ' ~ ', x1))
  }
  this_model <- lm ( my_formula, as.data.frame(model_data))
  this_model
}

possibly_run_researcher_models <- function(y) {
  purrr::possibly(run_researcher_models, otherwise=NULL)(params = y)

}

rerun_researcher_models <- function(
                         x, 
                         y, 
                         target_variable = 'count_per_thousand_researchers') {
  x1 = unlist(y)[1] 
  x2 = unlist(y)[2]
  
  model_data <- x  %>% 
    dplyr::select ( all_of(c("geo",
                      as.character(c(target_variable, x1,x2)))
    )) %>%
    tidyr::pivot_longer ( cols = -one_of('geo')) %>%
    dplyr::filter ( complete.cases(.)) %>%
    tidyr::pivot_wider ( names_from = 'name', 
                         values_from = 'value') %>%
    dplyr::filter ( complete.cases(.))
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( 'count_per_thousand_researchers ~', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( 'count_per_thousand_researchers ~', x1))
  }
  possibly(lm, NULL)(my_formula, data = model_data)
  
}

## Create parameter combinations ------------------------------------

researchers_dataset_scaled <- eurostat_eurobarometer_scaled %>%
  dplyr::select ( -all_of(c("count_per_million", 
                            "count_per_capita", 
                            "count_per_area", 
                            "count_per_pop_density", 
                            "count_per_researcher",
                            "count")) )

mt_researcher_model_parameters <- crossing(
  x1 = names(researchers_dataset_scaled)[-1],
  x2 = names(researchers_dataset_scaled)[-1] # [-1] is geo code 
  ) %>%
  rbind ( tibble ( x1 = names(researchers_dataset_scaled)[-1], 
                   x2 = NA_character_))  %>%
            filter ( x1 != x2,
                     ! x1 %in% c('geo', 'count', 'method'), 
                     ! x2 %in% c('geo', 'count', 'method'), 
                     ! grepl ( 'count', x1), 
                     ! grepl ( 'count', x2) ) 

## Results with outliers -----------------------------------------------

collect_researcher_results <- mt_researcher_model_parameters %>%
  rownames_to_column() %>%
  nest ( var_select = c(x1, x2) ) %>%
  dplyr::select (-"rowname") %>%
  mutate (  # get parameters to list
            params  = map(.x = var_select,  
                          .f = get_params) ) %>%
  mutate (  # and run researcher models
    models  = map(.x = params,     
                  .f = possibly_run_researcher_models)
  ) %>%
  filter ( #remove potentially errorneous results
           ! is.null(models)
           ) %>%
  mutate ( summaries = map (.x = models, 
                            .f=purrr::possibly(summary, NULL)), 
           all_significant = map(.x = summaries, 
                                 .f = purrr::possibly(is_all_significant, NULL )), 
           glanced = map(.x = models, 
                         .f=broom::glance),
           augmented  = map (.x = models, .f= broom::augment), 
           influental = map(.x = models, 
                            purrr::possibly(influence.measures, NULL)), 
           outliers = map(.x = influental, 
                          purrr::possibly(get_outlier_obs, NULL))) %>%
  mutate (data = map ( .x = outliers,  # create a data frame for next step
                       .f = possibly_without_outliers )
           )

## Results without outliers -------------------------------------------
collect_researcher_results_2 <- collect_researcher_results %>%  #rerun without outliers
  mutate ( #run the individual models with each possible selection of paramters
           models  = map2(.x = .$data, 
                          .y = .$params, 
                          rerun_researcher_models )
           ) %>%
  mutate ( # create the model summaries 
           summaries = map (.x = models, 
                            .f = summary)
           )  %>%
  mutate ( all_significant = map(
     .x = summaries, 
     .f = possibly( is_all_significant, otherwise = NULL) )) %>%
  mutate (  glanced =    map(.x = models, .f=broom::glance), 
            augmented  = map(.x = models, .f=broom::augment))

## Summary of results without outliers ---------------------------------
## Only variables where all parameters and intercept are significant ---
collect_researcher_results_3 <- collect_researcher_results_2 %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( all_significant ) %>%
  dplyr::select ( models )

collect_not_significant_researcher_results_3 <- collect_researcher_results_2 %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( ! all_significant ) %>%
  dplyr::select ( models )

## The data is in many nested data frames ------------------------ 
## Unnesting signicant models

coeffiecient_values_researchers <- collect_researcher_results_3$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

coefficient_values_not_significant <- collect_not_significant_researcher_results_3$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

unique(coeffiecient_values_researchers$names)
unique(coefficient_values_not_significant$names)

r_square_values_researchers_all_models  <- collect_researcher_results_2 %>% 
  unnest ( cols=c("all_significant", "glanced", "var_select" )) %>%
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

r_square_values_researchers  <- collect_researcher_results_2 %>% 
  unnest ( cols=c("all_significant", "glanced", "var_select" )) %>%
  filter ( all_significant ) %>%  ## only the significant models
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

researcher_model_coefficients  <- collect_researcher_results_2 %>%
  unnest ( all_significant ) %>%
  filter ( all_significant ) %>%
  dplyr::select ( var_select  ) %>%
  unnest ( var_select ) %>%
  rownames_to_column() %>%
  unite ( "model", x1:x2, sep = "+") %>%
  left_join ( r_square_values_researchers, by = 'model' ) %>%
  left_join ( coeffiecient_values_researchers, by = 'rowname') 


saveRDS(researcher_model_coefficients, 
        file.path('data',
                   'all_linreg_coefficients_count_per_thousand_researchers.rds')
        )
        