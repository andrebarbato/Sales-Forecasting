# Walmarte_Sales_Prediction
# Projeto apresentado como TCC no curso MBA Data Science e Analytics USP-ESALQ
# Autor: Andre Barbato
# Orientador Gustavo Lobo

# Carregando bibliotecas -------------------------------------------------

libs <- c("tidyverse",  # bibliotecas para data wrangling, graficos, e etc.
          "fable",      # biblioteca para modelagem de series temporais
          "fabletools",
          "feasts",
          "tsibble",    # biblioteca para time series tibble
          "ranger",
          "caret",
          "fastDummies",
          "ggcharts",
          "scales",
          "yardstick",
          "xgboost",
          "doParallel",
          "rnn"
          )

if(sum(as.numeric(!libs %in% installed.packages())) != 0){
  instalador <- libs[!libs %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(libs, require, character = T) 
} else {
  sapply(libs, require, character = T) 
}

# carregando as funções
source("4.functions.R")
