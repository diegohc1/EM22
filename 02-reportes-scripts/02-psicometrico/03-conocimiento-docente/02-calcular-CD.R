rm(list = ls())

# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Conocimiento pedagogico (CP) -> calcular
# *****************************************************************************************************************************

library(tidyverse)
library(here)
library(rio)
library(lavaan)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))
as.numeric.factor <- function(x) as.numeric(levels(x))[x]

# Generar excel con los indicadores de ajuste y las cargas factoriales #

# cargamos y acomodamos datos **********************
lista = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "01c-imputadas-conocimiento-doc", "*.rds")))

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

matriz_cfa <- matriz %>%
  #filter(Analisis2 %in% c("CFA", "PCA")) %>%
  drop_na(Cod_indice) %>%
  select(Concatena1, Constructo = Constructo_indicador, sub_escala, starts_with("Cod"), Enunciado, Analisis2) 

# 1. Docente de COMU ---------------------------------------------------
bdcom <- lista$EM2022_2SdocenteCOM_EBR_CD # base
matriz_cfa_com <- matriz_cfa %>%
  filter(Cod_indice == "DOC2SLEC_CODIDR") %>%
  mutate(cod_preg = paste0(cod_preg, "r"))

tabla_freq_columnas_dplyr(bdcom, matriz_cfa_com$cod_preg)

mod <- acomoda_string_lavaan(matriz_cfa_com)

bdcom <- bdcom %>%
  mutate(across(where(is.factor), as.numeric.factor)) %>%
  drop_na()

m1 <- cfa(mod, data = bdcom, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
reporte_lavaan(m1, puntajes = FALSE)

tabla_freq_columnas_dplyr(bdcom, matriz_cfa_com$cod_preg)
psych::polychoric(bdcom[matriz_cfa_com$cod_preg])

# 2. Docente de MATE: Conocimiento disciplinar -----------------------------------------------
bdmat <- lista$EM2022_2SdocenteMAT_EBR_CD # base
matriz_cfa_mat <- matriz_cfa %>%
  filter(Cod_indice == "DOC2SMAT_CODISF") %>%
  mutate(cod_preg = paste0(cod_preg, "r"))

mod <- acomoda_string_lavaan(matriz_cfa_mat)

bdmat <- bdmat %>%
  mutate(across(where(is.factor), as.numeric.factor)) %>%
  drop_na()

m1 <- cfa(mod, data = bdmat, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
reporte_lavaan(m1, puntajes = FALSE)


# 3. Docente de MATE: Conocimiento didáctico -----------------------------------------------
bdmat <- lista$EM2022_2SdocenteMAT_EBR_CD # base
matriz_cfa_mat <- matriz_cfa %>%
  filter(Cod_indice == "DOC2SMAT_CODIDF") %>%
  mutate(cod_preg = paste0(cod_preg, "r"))

mod <- acomoda_string_lavaan(matriz_cfa_mat)

bdmat <- bdmat %>%
  mutate(across(where(is.factor), as.numeric.factor)) %>%
  drop_na()

m1 <- cfa(mod, data = bdmat, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
reporte_lavaan(m1, puntajes = FALSE)
parameterestimates(m1)

tabla_freq_columnas_dplyr(bdmat, matriz_cfa_mat$cod_preg)
psych::polychoric(bdcom[matriz_cfa_com$cod_preg])

