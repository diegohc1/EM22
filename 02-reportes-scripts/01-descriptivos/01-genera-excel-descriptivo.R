# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Generar excel con proporciones de respuesta de cada item  
# *****************************************************************************************************************************

# Porcentaje de respuesta de cada item y segun estrato 
# ¿que estratos? 


rm(list = ls())
library(tidyverse)
library(here)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))


# (0) importamos bases de datos ----
lista = rio::import_list(Sys.glob(here("01-data", "02-con-etiquetas", "*.sav"))) %>% map(factorize)
  
# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

#mismas bases en miau y lista
matriz1 <- matriz %>% filter(Concatena1 %in% names(lista))
matriz1 <- filter(matriz1, TipoV %in% c("Categorico1", "Categorico2"))
lista <- keep(lista, names(lista) %in% matriz1$Concatena1) 

matriz_lista <- split(matriz1, matriz1$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

lista_vars <- matriz_lista %>% map(~.x[["cod_preg"]])
matriz_vs_lista(lista, lista_vars)

matriz_pegar <- bind_rows(matriz_lista) %>%
  select(Concatena1, Instrumento, cod_gen, cod_preg, Pregunta, Enunciado, TipoV, OpcionL) %>%
  drop_na(cod_gen)

# porque hay repetidos? 
# matriz_pegar <- distinct(matriz_pegar, Concatena1, cod_preg, .keep_all = TRUE)


# descriptivos por base 

# general ****
tab_general <- map2(lista, lista_vars, ~tabla_freq_columnas_dplyr(.x, nomvar = .y))

tab_general <- tab_general |>
  bind_rows(.id = "Concatena1") |>
  mutate(estrato = "General", tipo = "Nacional")

# por gestion ****
# tab_gestion <- map2(lista, lista_vars, 
#                     ~group_by(.x, gestion2) |> 
#                       tabla_freq_columnas_dplyr(nomvar = .y))
# 
# tab_gestion <- tab_gestion |>
#   bind_rows(.id = "Concatena2") |>
#   rename(estrato = gestion2) |>
#   mutate(tipo = "Gestion")
# 
# # por region ****
# tab_region <- map2(lista, lista_vars, 
#                    ~group_by(.x, nom_dre) |> 
#                      tabla_freq_columnas_dplyr(nomvar = .y))
# 
# tab_region <- tab_region |>
#   bind_rows(.id = "Concatena2") |>
#   rename(estrato = nom_dre) |>
#   mutate(tipo = "Region")
# 
# # juntamos los resultados
# tab_final <- bind_rows(tab_general, tab_gestion, tab_region) |> rename(cod_preg = var)

tab_final <- tab_general|> rename(cod_preg = var)

# pegar con la info de la matriz
tab_final2 <- left_join(tab_final, matriz_pegar, by = c("Concatena1", "cod_preg"))

# guardar!
# en .rdata
save(tab_final2, file = here("02-reportes-scripts", "01-descriptivos", "01-descriptivos-ffaa.Rdata"))

# en excel
tab_final_excel <- tab_final2 |>
  select(Instrumento, tipo, estrato, cod_gen, cod_preg, Pregunta, Enunciado, opcion, n, prop)

rio::export(tab_final_excel, here("02-reportes-scripts", "01-descriptivos", "01-descriptivos-ffaa.xlsx"))


# fin! 