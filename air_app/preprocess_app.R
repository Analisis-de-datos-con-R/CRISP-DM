# Preproces data for app --------------------------------------------------

# Load libraries
library(tidyverse)
library(zoo)
library(ggplot2)
library(plotly)
library(htmltools)

# Shiny
library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Tidymodeling
library(tidymodels)
library(modeltime)
library(modeltime.resample)

# Base Models
library(earth)
library(glmnet)
library(xgboost)
library(lightgbm)

# Core Packages
library(tidyverse)
library(lubridate)
library(timetk)
library(bonsai)

no2_zona <- read_rds("data/no2_zona.RDS")
nam_zonas <- no2_zona$zona %>% unique() %>% sort()

models <- list()

# el horizonte
h <- 21

# función auxilar para hacer los lags
lag_transformer <- function(data){
  data %>%
    tk_augment_lags(valor_zona, .lags = 1:h) %>%
    ungroup()
}

for(i in nam_zonas) {
  no2_zona_m30 <- no2_zona %>%
    filter(zona == i & anomaly == "No") %>% 
    ungroup() %>% 
    select(fecha, valor_zona)
  
  # Extensión de los datos en h
  no2_zona_m30_ext <- no2_zona_m30 %>%
    future_frame(
      .date_var = "fecha", 
      .length_out = h,
      .bind_data  = TRUE
    ) %>%
    ungroup()
  
  # Generación de variables
  no2_zona_m30_ext <- no2_zona_m30_ext %>% 
    mutate(
      fecha_num = normalize_vec(as.numeric(fecha)),
      dow = wday(fecha, week_start = 1), 
      month = month(fecha),
      quarter = quarter(fecha),
      year = year(fecha)
    ) %>%
    ungroup()
  
  # Añadir lags
  no2_zona_m30_ext <- no2_zona_m30_ext %>%
    lag_transformer()
  
  # Datos de entrenamiento
  train_data <- no2_zona_m30_ext %>%
    drop_na()
  
  # Datos para la prediccción
  future_data <- no2_zona_m30_ext %>%
    filter(is.na(valor_zona))
  
  # resample
  resamples_tscv <- time_series_cv(
    data        = train_data,
    assess      = h,
    initial     = 12 * 4 * h,
    skip        = 12 * 3 * h,
    slice_limit = 4
  )
  
  resample_plot <- resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(fecha, valor_zona, .facet_ncol = 2, .interactive = FALSE)
  
  # Recipe
  recipe_spec <- recipe(valor_zona ~ ., train_data)
  
  
  # Modelo 1 - glmnet
  
  # Especificación
  model_spec_glmnet <- linear_reg(penalty = 0.04) %>%
    set_engine("glmnet")
  
  # Ajuste
  workflow_fit_glmnet <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec %>% step_rm(fecha)) %>%
    fit(train_data) %>%
    recursive(
      transform  = lag_transformer,
      train_tail = tail(train_data, h))
  
  # Modelo 2 - earth
  
  # Especificación
  model_spec_mars <- mars(mode = "regression") %>%
    set_engine("earth")
  
  # Ajuste
  workflow_fit_mars <- workflow() %>%
    add_model(model_spec_mars) %>%
    add_recipe(recipe_spec) %>%
    fit(train_data) %>%
    recursive(
      transform  = lag_transformer,
      train_tail = tail(train_data, h))
  
  # Modelo 3 - LightGBM
  
  # Especificación
  model_spec_lightgbm <- boost_tree(mode = "regression",
                                    trees = 1000,
                                    min_n = 8,
                                    tree_depth = 10) %>%
    set_engine("lightgbm")
  
  # Ajuste
  workflow_fit_lightgbm <- workflow() %>%
    add_model(model_spec_lightgbm) %>%
    add_recipe(recipe_spec %>% step_rm(fecha)) %>%
    fit(train_data) %>%
    recursive(
      transform  = lag_transformer,
      train_tail = tail(train_data, h))
  
  # Modelo 4 - prophet_xgboost
  
  # Especificación
  model_spec_prophet_boost_log <- prophet_boost(
    mode = 'regression',
    changepoint_range = 0.8,
    logistic_floor = min(train_data$valor_zona),
    logistic_cap = max(train_data$valor_zona),
    growth = 'logistic',
    trees = 1000,
    min_n = 8,
    tree_depth = 10
  ) %>%
    set_engine("prophet_xgboost")
  
  # Ajuste
  workflow_fit_prophet_boost_log <- workflow() %>%
    add_model(model_spec_prophet_boost_log) %>%
    add_recipe(recipe_spec) %>%
    fit(train_data) %>%
    recursive(
      transform  = lag_transformer,
      train_tail = tail(train_data, h))
  
  # Model table
  
  models_tbl <- modeltime_table(
    workflow_fit_glmnet,
    workflow_fit_mars,
    workflow_fit_lightgbm,
    workflow_fit_prophet_boost_log
  )
  
  resamples_fitted <- models_tbl %>%
    modeltime_fit_resamples(
      resamples = resamples_tscv,
      control   = control_resamples(verbose = FALSE)
    )
  
  best_model <- modeltime_resample_accuracy(resamples_fitted) %>%
    arrange(mae) %>%
    select(.model_id) %>%
    head(1) 
  
  # Resample plot 
  
  models[[i]][["resample_plot"]] <-
    resamples_fitted %>%
      plot_modeltime_resamples(
        .point_size  = 3, 
        .point_alpha = 0.8,
        .interactive = FALSE
      )
  
  # Resample table
  models[[i]][["resample_table"]] <- resamples_fitted %>%
      modeltime_resample_accuracy(summary_fns = mean) %>%
      table_modeltime_accuracy(.interactive = FALSE)
  
  
  models[[i]][["forecast_plot"]] <- 
    models_tbl %>%
      filter(.model_id == best_model$.model_id) %>%
      modeltime_forecast(
        new_data    = future_data,
        actual_data = no2_zona_m30_ext,
        keep_data   = TRUE
      ) %>%
      filter(fecha >= "2023-01-01") %>% 
      plot_modeltime_forecast(
        .conf_interval_show = FALSE
      )
}


write_rds(models, "data/models.RDS")