rm(list = ls())

# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Conocimiento pedagogico (CP) -> acomodar e imputar
# *****************************************************************************************************************************

library(tidyverse)
library(here)
library(rio)
library(googlesheets4)
library(mice)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))
rowSumsNA <- function(x) rowSums(is.na(x)) #contar NA en las filas

# bases de datos 
d <- Sys.glob(here("01-data", "02-con-etiquetas", "*.sav"))

# **********************
# Docente de MATE ------
docmat = rio::import(d[str_detect(d, "MAT")]) 

v <- names(docmat)[endsWith(names(docmat), "r")]
docmat <- select(docmat, ID, cod_mod7, anexo, cor_minedu, dsc_seccion, all_of(v))

n_imp <- length(v) - length(v)*2/3 + 1 # por lo menos 2/3 de los datos completos

docmat_imputar <- docmat %>%
  mutate(nas = rowSumsNA(across(all_of(v)))) %>% #contamos NA
  filter(nas <= n_imp) %>% #nas con menos de 2/3 
  select(-nas)

docmat_imputar <- docmat_imputar %>%
  mutate(across(all_of(v), as.factor))

# verificamos: 
sapply(docmat_imputar, class)

dry <- mice(docmat_imputar, maxit = 0, print = F)
pred <- dry$predictorMatrix
meth <- dry$method

#excluir de la prediccion : 
pred[, c("ID", 'cod_mod7', 'anexo', "cor_minedu", 'dsc_seccion')] <- 0 
meth

inicio <- Sys.time()
temp.data <- mice(data = docmat_imputar, method = meth, predictorMatrix = pred, m = 1, seed = 700, maxit = 20)
final <- Sys.time()
final - inicio 

docmat_complete <- tibble(complete(temp.data, 1))
sapply(docmat_complete[v], function(x) mean(is.na(x)))

# **********************
# Docente de COM ------
doccom = rio::import(d[str_detect(d, "COM")]) 

v <- names(doccom)[endsWith(names(doccom), "r")]
doccom <- select(doccom, ID, cod_mod7, anexo, cor_minedu, dsc_seccion, all_of(v))

n_imp <- length(v) - length(v)*2/3 + 1 # por lo menos 2/3 de los datos completos

doccom_imputar <- doccom %>%
  mutate(nas = rowSumsNA(across(all_of(v)))) %>% #contamos NA
  filter(nas <= n_imp) %>% #nas con menos de 2/3 
  select(-nas)

doccom_imputar <- doccom_imputar %>% mutate(across(all_of(v), as.factor))

# verificamos: 
sapply(doccom_imputar, class)

dry <- mice(doccom_imputar, maxit = 0, print = F)
pred <- dry$predictorMatrix
meth <- dry$method

#excluir de la prediccion : 
pred[, c("ID", 'cod_mod7', 'anexo', "cor_minedu", 'dsc_seccion')] <- 0 
meth

inicio <- Sys.time()
temp.data <- mice(data = doccom_imputar, method = meth, predictorMatrix = pred, m = 1, seed = 700, maxit = 20)
final <- Sys.time()
final - inicio 

doccom_complete <- tibble(complete(temp.data, 1))
sapply(doccom_complete[v], function(x) mean(is.na(x)))

# guardamos las bases 

rio::export(doccom_complete, here("01-data", "03-intermedias", "01c-imputadas-conocimiento-doc", "EM2022_2SdocenteCOM_EBR_CD.rds"))
rio::export(docmat_complete, here("01-data", "03-intermedias", "01c-imputadas-conocimiento-doc", "EM2022_2SdocenteMAT_EBR_CD.rds"))



