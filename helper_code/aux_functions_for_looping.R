require("purrr")
require("dplyr")
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
df_no_outliers <- function (outliers, 
                            df = nuts2_dataset_scaled ) {
  
  df <- df %>%
    rownames_to_column() %>%
    left_join ( tibble::enframe( outliers ) %>%
                  rename ( rowname = name, 
                           outlier = value ), by ='rowname') %>%
    dplyr::filter ( ! outlier) %>%
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
