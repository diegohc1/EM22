rm(list = ls())

# *****************************************************************************************************************************
# Pegar puntajes a base del cuestionario
# *****************************************************************************************************************************

library(tidyverse)
library(here)
source(here("00-insumos", "0-funciones-apoyo.R"))
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")

# (1) pegar puntajes factoriales ----
# puntajes de escalas
lista_p = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "02a-puntajes-factoriales", "*.rds"))) 

# retirar estas columnas de las bases para no duplicar
retirar <- list(
  dir = c("cod_mod7", "anexo"), 
  doc = c("cod_mod7", "anexo", "cor_minedu"), 
  fam = c("cod_mod7", "anexo", "cor_minedu", "cor_est"),
  est = c("cod_mod7", "anexo")
)

# estas columnas estan en caso se quiera explorar los puntajes 
# pero las retiramos para no duplicar porque pegaremos con "ID"
fretira <- function(l, nom, ret){map_if(l, str_detect(names(l), nom), ~select(.x, -all_of(ret)))}
lista_p <- fretira(lista_p, "dir", retirar[[1]])
lista_p <- fretira(lista_p, "doc", retirar[[2]])
lista_p <- fretira(lista_p, "fam", retirar[[3]])
lista_p <- fretira(lista_p, "est", retirar[[4]])

# bd del cuestionario
lista <- rio::import_list(Sys.glob(here("01-data", "02-con-etiquetas", "*.sav")), setclass = "tibble") %>% map(rio::factorize)
lista <- lista[names(lista_p)] #quedarnos con las que tienen puntajes

matriz_vs_lista(names(lista), names(lista_p))

# puntajes a la base
llave <- c("ID")
lista_f <- map2(lista, lista_p, ~left_join(.x, .y, by = llave)) 
map(lista_f, names)

# cambiamos nombre en las de estudiante para no estar con .x o .y
lista_f <- lista_f %>%
  map_if(str_detect(names(.), "est"), 
         ~select(.x, -cor_minedu.y, -cor_est.y) %>%
           rename(cor_minedu = cor_minedu.x, cor_est = cor_est.x))

# (2) pegamos el ISE al estudiante/familia ----
lista_ise = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "02b-puntajes-ise", "*.rds"))) 
lista_ise[[1]] <- lista_ise[[1]] %>% select(ID, cod_mod7, cor_minedu, cor_est, ise2S)

# 2S dia1 
lista_f$EM2022_2Sestudiante_EBRD1 <- left_join(lista_f$EM2022_2Sestudiante_EBRD1, lista_ise[[1]], by = "ID")
lista_f$EM2022_2Sestudiante_EBRD1 <- lista_f$EM2022_2Sestudiante_EBRD1 %>% 
  select(-cor_minedu.y, -cor_est.y, -cod_mod7.y) %>%
  rename(cod_mod7 = cod_mod7.x, cor_minedu = cor_minedu.x, cor_est = cor_est.x) %>%
  group_by(cod_mod7) %>%
  mutate(isep2S = mean(ise2S, na.rm = TRUE))
  
# 2S dia2 
nrow(lista_f$EM2022_2Sestudiante_EBRD2)
nrow(lista_ise[[1]])

# hay repetidos, por ahora, saquemoslos....
lista_f$EM2022_2Sestudiante_EBRD2 <- lista_f$EM2022_2Sestudiante_EBRD2 %>%
  distinct(cor_minedu, cor_est, .keep_all = TRUE)

lista_ise[[1]] <- lista_ise[[1]] %>% distinct(cor_minedu, cor_est, .keep_all = TRUE)

lista_f$EM2022_2Sestudiante_EBRD2 <- left_join(lista_f$EM2022_2Sestudiante_EBRD2, lista_ise[[1]], by = c("cor_minedu", "cor_est"))

lista_f$EM2022_2Sestudiante_EBRD2 <- lista_f$EM2022_2Sestudiante_EBRD2 %>% 
  select(-ID.y, -cod_mod7.y) %>%
  rename(cod_mod7 = cod_mod7.x, ID = ID.x) %>%
  group_by(cod_mod7) %>%
  mutate(isep2S = mean(ise2S, na.rm = TRUE))


# fin! 

# exportamos todo:
rio::export_list(lista_f, here("01-data", "04-para-el-analisis", paste0(names(lista_f), ".sav")))



