rm(list = ls())

# *****************************************************************************************************************************
# Pegar puntajes a base del cuestionario
# *****************************************************************************************************************************

library(tidyverse)
library(here)
source(here("00-insumos", "0-funciones-apoyo.R"))

# (1) pegar puntajes factoriales 
# puntajes de escalas
lista_p = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "02-puntajes-factoriales", "*.rds"))) 

# retirar estas columnas de las bases para no duplicar
retirar <- list(
  dir = c("cod_mod7", "anexo"), 
  doc = c("cod_mod7", "anexo", "cor_minedu", "dsc_seccion"), 
  fam = c("cod_mod7", "anexo", "cor_minedu", "dsc_seccion_imp", "cor_est")
)
# estas columnas estan en caso se quiera explorar los puntajes 
# pero las retiramos para no duplicar porque pegaremos con "ID"
fretira <- function(l, nom, ret){map_if(l, str_detect(names(l), nom), ~select(.x, -all_of(ret)))}
lista_p <- fretira(lista_p, "dir", retirar[[1]])
lista_p <- fretira(lista_p, "doc", retirar[[2]])
lista_p <- fretira(lista_p, "fam|est", retirar[[3]])

# bd del cuestionario
lista <- rio::import_list(Sys.glob(here("01-data", "02-con-etiquetas", "*.sav")), setclass = "tibble") %>% map(factorize)
lista <- lista[names(lista_p)] #quedarnos con las que tienen puntajes

matriz_vs_lista(names(lista), names(lista_p))

# puntajes a la base
llave <- c("ID")
lista_f <- map2(lista, lista_p, ~left_join(.x, .y, by = llave))
# lista_f[[2]] %>% View()


# (2) pegamos el ISE al estudiante/familia
lista_ise = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "02b-puntajes-ise", "*.rds"))) 
lista_ise[[1]] <- lista_ise[[1]] %>% select(ID, -all_of(retirar$fam), ise2S)

lista_f$EM2022_2Sestudiante_EBRD1 <- left_join(lista_f$EM2022_2Sestudiante_EBRD1, lista_ise[[1]], by = "ID")
lista_f$EM2022_2Sestudiante_EBRD2 <- left_join(lista_f$EM2022_2Sestudiante_EBRD2, lista_ise[[1]], by = "ID")

lista_f$EM2022_2Sestudiante_EBRD2 %>% View()

# exportamos todo:

rio::export_list(lista_f, here("01-data", "04-para-el-analisis", paste0(names(lista_f), ".sav")))

# aqui tambien deberiamos pegar el ISE tambien! 






