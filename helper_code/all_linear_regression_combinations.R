library(tidyverse)
library(broom)
## loading file created by helper_code/prepare_data.R
load (file = file.path("data", "all_data_for_analysis.rda"))

## load auxilliary functions for looping
source(file.path("helper_code", "aux_functions_for_looping.R"))

var_names <- names ( eurostat_eurobarometer_scaled )

targets <- var_names[ grepl("count", var_names)]
preds   <- var_names[ ! var_names %in% c(targets, "geo") ]

regression_model_parameters <-   crossing(
  ## add all potential multiple regressions with two predictors
  y  = c("count", "count_per_area", "count_per_thousand_researchers"), 
  x1 = preds,
  x2 = NA_character_ )

mt_model_parameters <- crossing(
  ## add all potential multiple regressions with two predictors
   y  = c("count", "count_per_area", "count_per_thousand_researchers"), 
   x1 = preds,
   x2 = preds
  ) %>%
  filter ( # remove y = x1 + x1 type combinations
    x1 != x2 )  %>%
  bind_rows ( regression_model_parameters ) %>%
  mutate ( model_formula = case_when(
    is.na(x2) ~ paste0(y, " ~ ", x1), 
    TRUE ~ paste0(y, " ~ ", x1, " + ", x2)
  ) ) 


model_formula <- mt_model_parameters$model_formula[2]

run_lm_models <- function(
  model_formula, 
  model_df =  eurostat_eurobarometer_scaled ) {

  this_model <- lm ( as.formula(model_formula),
                     data = model_df)
  this_model
}

run_lm_models_without_outliers <- function(
  model_formula,
  outlier_list,
  model_df =  eurostat_eurobarometer_scaled ) {
  
  model_formula <- unlist(model_formula)
  outlier_list <- unlist(outlier_list)
  
  model_without_outliers <- lm (
    as.formula(model_formula),
    data = model_df[ which(!outlier_list),] 
    )
  
  model_without_outliers
}


## Results with outliers -----------------------------------------------

all_nested_models <- mt_model_parameters  %>%
  rownames_to_column() %>%
  nest ( ., model_formula = all_of("model_formula")  ) %>%
  dplyr::select (-"rowname") %>%
  mutate ( #run all lm models, on error make result NULL 
          models = map (.x = unlist(model_formula), 
                        .f = possibly(run_lm_models, NULL))
  ) 

all_identified_outliers <- all_nested_models %>%
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
                          purrr::possibly(get_outlier_obs, NULL)))


results_without_outliers <- all_identified_outliers %>%
  mutate ( final_models  =
    map2( 
       .x = model_formula, 
       .y = outliers, 
       .f = possibly(run_lm_models_without_outliers, NULL))
  )

summarize_final_results <- results_without_outliers %>%
  mutate ( # create the model summaries 
    summaries = map (.x = final_models, 
                     .f = summary)
  )  %>%
  mutate ( all_significant = map(
    .x = summaries, 
    .f = possibly( is_all_significant, otherwise = NULL) )) %>%
  mutate (  ## add model summaries to a tidy row:
            glanced =    map(.x = final_models, .f=broom::glance),
            ## add information about each observation:
            augmented  = map(.x = final_models, .f=broom::augment), 
            
            tidied =     map (.x = final_models , .f=broom::tidy )
            )


significant_model_summaries <- summarize_final_results %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( 
    ## select models where all coefficient(s) are significant 
    all_significant ) %>%
  dplyr::select ( y, x1, x2, glanced ) %>%
  unnest ( glanced ) %>%
  arrange ( -adj.r.squared )

tidy_review_all_significant_models <- summarize_final_results %>%
  unnest ( all_significant ) %>%
  dplyr::filter ( 
    ## select models where all coefficient(s) are significant 
    all_significant ) %>%
  dplyr::select ( y, model_formula, tidied ) %>%
  unnest ( ., cols = all_of(c("model_formula", "tidied"))   ) %>%
  mutate (model_formula = as.character(unlist(model_formula))) %>%
  distinct (., estimate, .keep_all = TRUE) %>%
  arrange ( -estimate ) %>%
  filter ( ! grepl("gdp_pps + gdp_pps_hab", model_formula), 
           ! grepl("internet_purchases_pc + internet_use_banking_pc", model_formula), )

save ( tidy_review_all_significant_models, 
       significant_model_summaries , 
       file = file.path("data", "lin_model_summaries.rda"))

review_coefficients_plot <- tidy_review_all_significant_models %>%
  filter ( term != "(Intercept)") %>%
  ggplot ( ., aes ( y = forcats::fct_reorder (term,
                                              .x = estimate, 
                                              .desc = FALSE), 
                    x = estimate, 
                    color = statistic, 
                    group = y)) +
  geom_point( ) +
  scale_x_log10() +
  scale_color_gradient(low = 'darkblue', high = 'grey90') +
  facet_wrap ( facets = 'y', 
               ncol = 3, 
               scales = 'free_x') +
  theme ( legend.position = "bottom") +
  labs ( fill = "Explained variable", 
         x = "coefficient size (log)", 
         y = "variable name", 
         title = "Coefficient Sizes In Linear (Multiple) Regression Models")

save ( tidy_review_all_significant_models, 
       significant_model_summaries , 
       review_coefficients_plot,
       file = file.path("data", "lin_model_summaries.rda"))
