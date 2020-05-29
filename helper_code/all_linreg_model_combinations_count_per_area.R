library(tidyverse)
library(broom)
## loading file created by helper_code/prepare_data.R
load (file = file.path("data", "all_data_for_analysis.rda"))

## load auxilliary functions for looping
source(file.path("helper_code", "aux_functions_for_looping.R"))


## Run the models
run_count_per_area_models <- function(params, 
                       target_variable = 'count_per_area', 
                       model_df = count_per_area_scaled) {
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

possibly_run_count_per_area_models <- function(y) {
  purrr::possibly(run_count_per_area_models, otherwise=NULL)(params = y)

}

rerun_count_per_area_models <- function(x, y, 
                         target_variable = 'count_per_area') {
  x1 = unlist(y)[1] 
  x2 = unlist(y)[2]
  
  model_data <- x  %>% 
    dplyr::select ( all_of(c("geo",
                      as.character(c(target_variable, x1,x2)))
    )) %>%
    tidyr::pivot_longer ( cols = -one_of('geo')) %>%
    dplyr::filter ( complete.cases(.)) %>%
    tidyr::pivot_wider ( names_from = 'name', values_from = 'value') %>%
    dplyr::filter ( complete.cases(.))
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( 'count_per_area ~', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( 'count_per_area ~', x1))
  }
  possibly(lm, NULL)(my_formula, data = model_data)
  
}

## Create parameter combinations ------------------------------------

count_per_area_scaled <- eurostat_eurobarometer_scaled %>%
  dplyr::select ( -all_of(c("count_per_million", 
                            "count_per_researcher", 
                            "count_per_capita", 
                            "count_per_thousand_researchers", 
                            "count_per_pop_density", 
                            "count")) )

mt_count_per_area_model_parameters <- crossing(
  x1 = names(count_per_area_scaled)[-1],  #[-1] is geo code
  x2 = names(count_per_area_scaled)[-1]
  ) %>%
  rbind ( tibble ( x1 = names(count_per_area_scaled)[-1], 
                   x2 = NA_character_))  %>%
            filter ( x1 != x2,
                     ! x1 %in% c('geo', 'count_per_area', 'method'), 
                     ! x2 %in% c('geo', 'count_per_area', 'method'), 
                     ! grepl ( 'count', x1), 
                     ! grepl ( 'count', x2) ) 

## Results with outliers ------------------------------------------
collect_count_per_area_results <- mt_count_per_area_model_parameters %>%
  rownames_to_column() %>%
  nest ( var_select = c(x1, x2) ) %>%
  dplyr::select (-"rowname") %>%
  mutate ( params  = map(var_select,  .f=get_params),   #select params and run models
           models  = map(.x = params, .f=possibly_run_count_per_area_models)) %>%
  mutate ( summaries = map (.x = models, 
                            .f=purrr::possibly(summary, NULL)), 
           all_significant = map(.x = summaries, 
                                 .f = purrr::possibly(is_all_significant,
                                                      NULL )), 
           glanced = map(.x = models, 
                         .f=broom::glance),
           augmented  = map (.x = models, .f= broom::augment), 
           influental = map(.x = models, 
                            purrr::possibly(influence.measures, NULL)), 
           outliers = map(.x = influental, 
                          purrr::possibly(get_outlier_obs, NULL))
           ) %>%
  mutate (data = map2 ( 
    .x = outliers,  # create a data frame for next step
    .y = count_per_area_scaled,
    .f = possibly_without_outliers )
           )


## Results without outliers -------------------------------------------
collect_count_per_area_results_2 <- collect_count_per_area_results %>%  #rerun without outliers
  mutate ( models  = map2(.x = .$data, 
                          .y =.$params, 
                          rerun_rs ),
           summaries = map (.x = models,
                            .f=summary))  %>%
  mutate ( all_significant = map(
    .x = summaries,
    .f = possibly( is_all_significant, NULL ))
    ) %>%
  mutate (  glanced = map(.x = models, .f=broom::glance), 
            augmented  = map (.x = models, .f= broom::augment))

## Summary of results without outliers ---------------------------------
## Only variables where all parameters and intercept are significant ---
collect_count_per_area_results_3 <- collect_count_per_area_results_2  %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( all_significant ) %>%
  dplyr::select ( models )

collect_count_per_area_not_significant_results <- collect_count_per_area_results_2  %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( ! all_significant ) %>%
  dplyr::select ( models )

## The data is in many nested data frames ------------------------ 
## Unnesting signicant models

coefficient_values_count_per_area <- collect_count_per_area_results_3$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

not_significant_coefficient_values_count_per_area <- collect_count_per_area_not_significant_results$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

unique(coefficient_values_count_per_area $names)                  # significant coefficients
unique(not_significant_coefficient_values_count_per_area$names)  # not significant coefficients

r_square_values_count_per_area_all  <- collect_count_per_area_results_2  %>% 
  unnest ( cols=c("all_significant", "glanced", "var_select" )) %>%
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

r_square_values_count_per_area  <- collect_count_per_area_results_2  %>% 
  unnest ( cols=c("all_significant", "glanced", "var_select" )) %>%
  filter ( all_significant == TRUE ) %>%
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

coefficients_count_per_area  <- collect_count_per_area_results_2 %>%
  unnest ( all_significant ) %>%
  filter ( all_significant ) %>%
  dplyr::select ( var_select  ) %>%
  unnest ( var_select ) %>%
  rownames_to_column() %>%
  unite ( "model", x1:x2, sep = "+") %>%
  left_join ( r_square_values_count_per_area, by = 'model' ) %>%
  left_join ( coefficient_values_count_per_area, by = 'rowname') %>%
  arrange ( -adj.r.squared )


saveRDS(coefficients_count_per_area,
        file.path('data',
                  'all_linreg_coefficients_count_per_area.rds')
        )
        