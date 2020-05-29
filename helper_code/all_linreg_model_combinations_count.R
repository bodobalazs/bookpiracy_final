library(tidyverse)
library(broom)
## loading file created by helper_code/prepare_data.R
load (file = file.path("data", "all_data_for_analysis.rda"))

## load auxilliary functions for looping
source(file.path("helper_code", "aux_functions_for_looping.R"))

## Run the models
run_models <- function(params, 
                       target_variable = 'count', 
                       model_df = nuts2_dataset_scaled) {
  x1 = unlist(params)[1] 
  x2 = unlist(params)[2]
  
  vars_to_select <- as.character(
    c("geo", target_variable, x1,x2)
  )
  
  model_data <- model_df  %>% 
    dplyr::select ( tidyselect::all_of(vars_to_select) ) %>%
    tidyr::pivot_longer ( cols = -one_of('geo')) %>%
    dplyr::filter ( complete.cases(.)) %>%
    tidyr::pivot_wider ( names_from = 'name', 
                         values_from = 'value') %>%
    dplyr::filter ( complete.cases(.))
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( target_variable, ' ~ ', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( target_variable, ' ~ ', x1))
  }
  this_model <- lm ( my_formula, as.data.frame(model_data))
  this_model
}

possibly_run_models <- function(y) {
  purrr::possibly(run_models, otherwise=NULL)(params = y)

}

rerun_models <- function(x, y, 
                         target_variable = 'count') {
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
    my_formula <- as.formula ( paste0(target_variable, ' ~ ', x1, "+", x2) )
  } else {
    my_formula <- as.formula ( paste0(target_variable, ' ~ ', x1) )
  }
  possibly(lm, NULL)(my_formula, data = model_data)
  
}

## Create parameter combinations ------------------------------------

count_dataset_scaled <- eurostat_eurobarometer_scaled %>%
  dplyr::select ( -all_of(c("count_per_million", 
                            "count_per_researcher", 
                            "count_per_capita", 
                            "count_per_thousand_researchers", 
                            "count_per_pop_density", 
                            "count_per_area")) )

mt_count_model_parameters <- crossing(
  x1 = names(count_dataset_scaled)[-1], 
  x2 = names(count_dataset_scaled)[-1]) %>%
  rbind ( tibble ( x1 = names(count_dataset_scaled)[-1], 
                   x2 = NA_character_))  %>%
            filter ( x1 != x2,
                     ! x1 %in% c('geo', 'count', 'method'), 
                     ! x2 %in% c('geo', 'count', 'method'), 
                     ! grepl ( 'count', x1), 
                     ! grepl ( 'count', x2) ) 

## Results with outliers ------------------------------------------
collect_count_results <- mt_count_model_parameters %>%
  tibble::rownames_to_column() %>%
  nest ( var_select = c(x1, x2) ) %>%
  dplyr::select (-all_of("rowname")) %>%
  dplyr::mutate ( params = purrr::map(var_select, 
                                      .f=get_params)) %>%
  dplyr::mutate ( models  = purrr::map(.x = params, 
                                       .f=possibly_run_models)) %>%
  dplyr::mutate ( summaries = purrr::map (
                     .x = models, 
                     .f=purrr::possibly(summary, 
                                        otherwise = NULL))) %>%
  dplyr::mutate ( all_significant = purrr::map(
                    .x = summaries, 
                    .f = purrr::possibly(is_all_significant,
                                         otherwise = NULL ))) %>%
  dplyr::mutate ( glanced = map(.x = models, 
                                .f = broom::glance)) %>%
  dplyr::mutate (augmented  = purrr::map (.x = models, 
                                          .f = broom::augment) ) %>%
  dplyr::mutate ( influental = purrr::map(.x = models, 
                                          purrr::possibly(influence.measures, NULL)), 
                  outliers = purrr::map(.x = influental, 
                                        .f = purrr::possibly(
                                          get_outlier_obs, NULL)))%>%
  mutate (data = purrr::map ( 
    .x = outliers,  # create a data frame for next step
    .f = possibly_without_outliers )
           )

## Results without outliers -------------------------------------------
collect_count_results_2 <- collect_count_results %>%  #rerun without outliers
  mutate ( models  = map2(.x = .$data, 
                          .y =.$params, 
                          rerun_models ),
           summaries = map (.x = models, .f=summary))  %>%
  mutate ( 
    all_significant = map(
    .x = summaries, 
    .f = possibly(is_all_significant, NULL))
    ) %>%
  mutate (  glanced    = map(.x = models, .f=broom::glance), 
            augmented  = map(.x = models, .f= broom::augment))

## Summary of results without outliers ---------------------------------
## Only variables where all parameters and intercept are significant ---
collect_count_results_3 <- collect_count_results_2 %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( all_significant ) %>%
  dplyr::select ( models )

collect_count_results_3_not_significant <- collect_count_results_2  %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( ! all_significant ) %>%
  dplyr::select ( models )

## The data is in many nested data frames ------------------------ 
## Unnesting signicant models

coefficient_values_count <- collect_count_results_3$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

coefficient_values_not_significant_count <- collect_count_results_3_not_significant$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

r_square_values_all  <- collect_count_results_2 %>% 
  unnest ( cols=c("all_significant", "glanced", "var_select" )) %>%
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

r_square_values  <- collect_count_results_2 %>% 
  unnest ( cols=c("all_significant", "glanced", "var_select" )) %>%
  filter ( all_significant == TRUE ) %>%
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

coefficients_count  <- collect_count_results_2 %>%
  unnest ( all_significant ) %>%
  filter ( all_significant ) %>%
  dplyr::select ( var_select  ) %>%
  unnest ( var_select ) %>%
  rownames_to_column() %>%
  unite ( "model", x1:x2, sep = "+") %>%
  left_join ( r_square_values_count,    by = 'model' ) %>%
  left_join ( coefficient_values_count, by = 'rowname') %>%
  arrange ( -adj.r.squared )


saveRDS(coefficients_count, 
        file.path('data',
                  'all_linreg_coefficients_count.rds')
        )
        