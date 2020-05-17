library(tidyverse)
library(broom)
load (file = file.path("data", "all_data_for_analysis.rda"))

## Auxilliary functions for looping ----------------------
is_all_significant <- function(summaries, critical_value = 1.96) {
  m <- summaries$coefficients
  all ( abs(m[,3]) > 1.96)
}


get_outlier_obs <- function(m) {
  m$is.inf[,6] 
}

df_no_outliers <- function ( df = mtcars, outliers ) {
  
  df <- df %>%
    rownames_to_column() %>%
    left_join ( tibble::enframe( outliers ) %>%
                  rename ( rowname = name, 
                           outlier = value ), by ='rowname') %>%
    filter ( ! outlier) %>%
    dplyr::select (-outlier)
}


get_params <- function(df) {
  as.list(df)
}


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

#CHANGE HERE THE DEPENDENT VARIABLES IF NEEDED!
run_models <- function(y) {
  x1 = unlist(y)[1] 
  x2 = unlist(y)[2]
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( 'count_per_capita ~', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( 'count_per_capita ~', x1))
  }
  lm ( my_formula, as.data.frame(eurostat_eurobarometer_data))
}


rerun_models <- function(x, y) {
  x1 = unlist(y)[1] 
  x2 = unlist(y)[2]
  
  if ( !is.na(x2) ) {
    my_formula <- as.formula ( paste0( 'count_per_area ~', x1, "+", x2))
  } else {
    my_formula <- as.formula ( paste0( 'count_per_area ~', x1))
  }
  lm ( my_formula, x)
  
}


names (  eurostat_eurobarometer_data)
## Create parameter combinations ------------------------------------

nuts2_dataset_scaled <- eurostat_eurobarometer_data %>%
  dplyr::select ( -starts_with("count_per") )

mt_model_parameters <- crossing(x1 = names(nuts2_dataset_scaled)[-1], 
                                x2= names(nuts2_dataset_scaled)[-1])%>%
  rbind ( tibble ( x1 = names(nuts2_dataset_scaled)[-1], 
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
           models  = map(.x = params, .f=run_models),
           summaries = map (.x = models, .f=summary),
           all_significant = map(.x = summaries, .f =is_all_significant ),
           glanced = map(.x = models, .f=broom::glance), 
           augmented  = map (.x = models, .f= broom::augment), 
           influental = map(.x = models, influence.measures), 
           outliers = map(.x = influental, get_outlier_obs), 
           data = map ( .x = outliers, ~df_no_outliers(df = eurostat_eurobarometer_data, 
                                                       outliers = .x))
  )


## Results without outliers -------------------------------------------
collect_results_2 <- collect_results %>%
  mutate ( models  = map2(.x = .$data, .y =.$params, rerun_models ), 
           summaries = map (.x = models, .f=summary),
           all_significant = map(.x = summaries, .f =is_all_significant ),
           glanced = map(.x = models, .f=broom::glance), 
           augmented  = map (.x = models, .f= broom::augment)) 



## Summary of results without outliers ---------------------------------
## Only variables where all parameters and intercept are significant ---
collect_results_3 <- collect_results_2 %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( all_significant ) %>%
  dplyr::select ( models )



## The data is in many nested data frames ------------------------ 
## Unnesting signicant models

collect_results_3$models[[1]]$coefficients

coefficient_values <- collect_results_3$models %>%
  map_dfr( ~ as.data.frame(t(as.matrix(coef(.))))) %>%
  rownames_to_column() %>%
  gather ( names, values, !!3:ncol(.)) %>%
  filter ( !is.na(values) )

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
        