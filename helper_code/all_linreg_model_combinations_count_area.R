library(tidyverse)
library(broom)
load (file = file.path("data", "all_data_for_analysis.rda"))

## Auxilliary functions for looping ----------------------
## Collecting signficance values 


is_all_significant <- function(summaries, critical_value = 1.96) {
  m <- summaries$coefficients
  all ( abs(m[,3]) > 1.96)
}

## Get outlier observations
get_outlier_obs <- function(m) {
  m$is.inf[,6] 
}

## Remove outliers
df_no_outliers <- function (outliers , df = nuts2_dataset_scaled ) {
  
  df <- df %>%
    rownames_to_column() %>%
    left_join ( tibble::enframe( outliers ) %>%
                  rename ( rowname = name, 
                           outlier = value ), by ='rowname') %>%
    filter ( ! outlier) %>%
    dplyr::select (-outlier)
}

possibly_without_outliers <- function(outliers) {
  
  possibly_no_outliers <- purrr::possibly( 
  .f = df_no_outliers, otherwise = NULL)
  possibly_no_outliers(outliers) 

  }


## Get model parameters
get_params <- function(df) {
  as.list(df)
}

## Fit the model
fit_model <- function (...) {
  
  args <- list(...)
  arg_names <- names(...)
  
  if ( "data" %in% arg_names ) { 
    df = args[[1]]$data
  } else df <- mtcars
  
  if ( "y" %in% arg_names ) { 
    y = args[[1]]$y
  } else y <- 'mpg'
  
  
  x1 = args[[1]]$x1
  x2 <- NA_character_
  x3 <- NA_character_
  x2 <- args[[1]]$x2
  my_formula <- paste0 (y, " ~ ", x1)
  
  if ("x2" %in% arg_names)  { 
    x2 <- args[[1]]$x2
    if(!is.na(x2)) {
      my_formula <- paste0(my_formula, ' + ', x2)}
  }
  
  if ("x3" %in% arg_names) { 
    x3 <- args[[1]]$x3
    if(!is.na(x3)) {
      my_formula <- paste0(my_formula, ' + ', x3)}
  }
  
  
  lm ( as.formula(my_formula), data = df)
  
}

y = collect_results_2$params[[18]]

## Run the models
run_models <- function(y, 
                       target_variable = 'count_per_area', 
                       model_df = nuts2_dataset_scaled) {
  x1 = unlist(y)[1] 
  x2 = unlist(y)[2]
  
  model_data <- model_df  %>% 
    select ( all_of(c("geo",
                      as.character(c(target_variable, x1,x2)))
                    )) %>%
    pivot_longer ( cols = -one_of('geo')) %>%
    filter ( complete.cases(.)) %>%
    pivot_wider ( names_from = 'name', values_from = 'value') %>%
    filter ( complete.cases(.))
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( target_variable, ' ~ ', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( target_variable, ' ~ ', x1))
  }
  this_model <- lm ( my_formula, as.data.frame(model_data))
  this_model
}

possibly_run_models <- function(y) {
  purrr::possibly(run_models, otherwise=NULL)(y)

}

rerun_models <- function(x, y, 
                         target_variable = 'count_per_area') {
  x1 = unlist(y)[1] 
  x2 = unlist(y)[2]
  
  model_data <- x  %>% 
    select ( all_of(c("geo",
                      as.character(c(target_variable, x1,x2)))
    )) %>%
    pivot_longer ( cols = -one_of('geo')) %>%
    filter ( complete.cases(.)) %>%
    pivot_wider ( names_from = 'name', values_from = 'value') %>%
    filter ( complete.cases(.))
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( 'count_per_area ~', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( 'count_per_area ~', x1))
  }
  possibly(lm, NULL)(my_formula, data = model_data)
  
}

## Create parameter combinations ------------------------------------

nuts2_dataset_scaled <- eurostat_eurobarometer_scaled %>%
  dplyr::select ( -all_of(c("count_per_million", 
                            "count_per_researcher", 
                            "count_per_capita", 
                            "count_per_thousand_researchers", 
                            "count_per_pop_density")) )

mt_model_parameters <- crossing(x1 = names(eurostat_eurobarometer_scaled)[-1], 
                                x2 = names(eurostat_eurobarometer_scaled)[-1])%>%
  rbind ( tibble ( x1 = names(eurostat_eurobarometer_scaled)[-1], 
                   x2 = NA_character_))  %>%
            filter ( x1 != x2,
                     ! x1 %in% c('geo', 'count', 'method'), 
                     ! x2 %in% c('geo', 'count', 'method'), 
                     ! grepl ( 'count', x1), 
                     ! grepl ( 'count', x2) ) 

## Results with outliers ------------------------------------------
collect_results <- mt_model_parameters %>%
  rownames_to_column() %>%
  nest ( var_select = c(x1, x2) ) %>%
  dplyr::select (-"rowname") %>%
  mutate ( params  = map(var_select,  .f=get_params), 
           models  = map(.x = params, .f=possibly_run_models), 
           summaries = map (.x = models, 
                            .f=purrr::possibly(summary, NULL)), 
           all_significant = map(.x = summaries, 
                                 .f = purrr::possibly(is_all_significant, NULL )), 
           glanced = map(.x = models, 
                         .f=broom::glance),
           augmented  = map (.x = models, .f= broom::augment), 
           influental = map(.x = models, 
                            purrr::possibly(influence.measures, NULL)), 
           outliers = map(.x = influental, 
                          purrr::possibly(get_outlier_obs, NULL)), 
           data = map ( .x = outliers,
                        .f= possibly_without_outliers )
           )

collect_results$summaries[[18]]
collect_results$outliers[[18]]

## Results without outliers -------------------------------------------
collect_results_2 <- collect_results %>%
  mutate ( models  = map2(.x = .$data, .y =.$params, rerun_models ),
           summaries = map (.x = models, .f=summary))  %>%
  mutate ( all_significant = map(.x = summaries, .f = possibly(
                 is_all_significant, NULL ))) %>%
  mutate (  glanced = map(.x = models, .f=broom::glance), 
            augmented  = map (.x = models, .f= broom::augment))

is_all_significant(collect_results$summaries[[18]])

## Summary of results without outliers ---------------------------------
## Only variables where all parameters and intercept are significant ---
collect_results_3 <- collect_results_2 %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( all_significant ) %>%
  dplyr::select ( models )

collect_results_3_not_significant <- collect_results_2 %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( ! all_significant ) %>%
  dplyr::select ( models )

## The data is in many nested data frames ------------------------ 
## Unnesting signicant models

coefficient_values <- collect_results_3$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

coefficient_values_not_significant <- collect_results_3_not_significant$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

unique(coefficient_values$names)
unique(coefficient_values_not_significant$names)

r_square_values  <- collect_results_2 %>% 
  unnest ( cols=c(all_significant, glanced, var_select )) %>%
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

r_square_values  <- collect_results_2 %>% 
  unnest ( cols=c(all_significant, glanced, var_select )) %>%
  filter ( all_significant ) %>%
  unite ( "model", x1:x2, sep = "+") %>%
  dplyr::select ( model, r.squared, adj.r.squared )

coefficients  <- collect_results_2 %>%
  unnest ( all_significant ) %>%
  filter ( all_significant ) %>%
  dplyr::select ( var_select  ) %>%
  unnest ( var_select ) %>%
  rownames_to_column() %>%
  unite ( "model", x1:x2, sep = "+") %>%
  left_join ( r_square_values, by = 'model' ) %>%
  left_join ( coefficient_values , by = 'rowname') 


saveRDS(coefficients, file.path('data-raw','all_linreg_coefficients.rds' ))
        