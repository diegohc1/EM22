rm(list = ls())

# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Indice socioeconomico (ISE) -> acomodar e imputar
# *****************************************************************************************************************************

library(tidyverse)
library(here)
library(rio)
library(googlesheets4)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))

# bases de datos 
lista = rio::import_list(Sys.glob(here("01-data", "02-con-etiquetas", "*.sav"))) 
d <- Sys.glob(here("01-data", "02-con-etiquetas", "*.sav"))

# solo ise
bdise <- c("EM2022_2Sestudiante_EBRD1", "EM2022_2Pfamilia_EBR", "EM2022_4Pfamilia_EBR")
d <- d[str_detect(d, paste(bdise, collapse = "|"))]

lista = rio::import_list(d) %>% set_names(bdise[1])

dd <- lista[[1]]

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")
matriz_ise <- filter(matriz, str_detect(Cod_indice, "ISE"))
unique(matriz_ise$Concatena1)

# variables del ise - miau
vars_ise <- matriz_ise %>%
  select(Concatena1, cod_preg, Constructo = Constructo_indicador) %>%
  split(.$Concatena1) %>%
  map(~pull(.x, cod_preg))

# para cambiar los nombres de los items del ise
link_ise_recod <- "https://docs.google.com/spreadsheets/d/1-EYBLulxG0zwRLdK-Q36J7NDQZlwuJkQ3b_iMATMB_I/edit#gid=0"

ise_recod <- link_ise_recod %>%
  sheet_names() %>%
  set_names() %>%
  map(read_sheet, ss = link_ise_recod) %>%
  set_names(bdise)

# ordenar la info según lista
vars_ise <- keep(vars_ise, names(vars_ise) %in% names(lista))
vars_ise <- vars_ise[names(lista)]

ise_recod <- keep(ise_recod, names(ise_recod) %in% names(lista))
ise_recod <- ise_recod[names(lista)]

# nos quedamos con las variables 
lista_ise <- lista %>%
  map2(., vars_ise, 
       ~select(.x, ID, cod_mod7, anexo, cor_minedu, cor_est, all_of(.y)) %>%
         mutate(across(all_of(.y), as.numeric)))


# cambiamos nombres! 
lista_iseb <- map2(lista_ise, ise_recod, function(x, y) rename_with(x, ~y[["cod"]], y[["a2022"]]))

# (1) recodificamos  ----

# 1. nivel educativo -----
# recod_edu_fam <- set_names(c(0, 3, 6, 8.5, 11, 7.5, 9, 12.5, 14, 13.5, 16, 19), 1:12)
recod_edu_est <- set_names(c(0, 3, 6, 8.5, 11, 12.5, 14, 13.5, 16, 19), 1:10)

# 2. materiales de vivienda ----

# pared
# base = etiqueta = nueva
# 1 = Ladrillo o bloque de cemento = 5
# 2 = Piedra o sillar con cal o cemento = 2
# 3 = Adobe o tapia = 4
# 4 = Quincha (caña con barro) = 2
# 5 = Piedra con barro = 2
# 6 = Madera o tablas = 3
# 7 = Esteras = 1
# 8 = Otro = 1
recod_pared <- set_names(c(5, 2, 4, 2, 2, 3, 1, 1), 1:8)

# techos
# base = etiqueta = nueva
# 1 = Concreto armado (cemento y ladrillo) = 7
# 2 = Madera = 5
# 3 = Tejas = 6
# 4 = Plancha de calamina, fibra de cemento o similares = 4
# 5 = Caña o estera con barro = 3
# 6 = Esteras = 2
# 7 = Paja u hojas de palmera = 4
# 8 = Otro = 1
recod_techo <- set_names(c(7, 5, 6, 4, 3, 2, 4, 1), 1:8)

# pisos 
# base = etiqueta = nueva
# 1 = Parquet o madera pulida = 6
# 2 = Pisos asfalticos, vinilicos o similares = 5
# 3 = Losetas, mayolicas, terrazos o similares = 4
# 4 = Madera (entablado) = 3
# 5 = Cemento = 2
# 6 = Tierra = 1
# 7 = Otro = 1
recod_piso <- set_names(c(6, 5, 4, 3, 2, 1, 1), 1:7)

# 3. servicios basicos
# agua... 1 = Del caño dentro de la casa = 1, todo lo demas 0
# desague... 1 = El baño está dentro de la casa y podemos jalar la palanca o cadena = 1, todo lo demas 0
# luz... 1 = Electricidad = 1, todo lo demas 0

# 4. activos y otros servicios: no tiene = 0, tiene = 1

# entonces, recodificamos: 
lista_iseb_r <- lista_iseb %>%
  map(~mutate(.x, 
              across(starts_with("edu"), ~recode(.x, !!!recod_edu_est)),
              matpar = recode(matpar, !!!recod_pared),
              mattec = recode(mattec, !!!recod_techo),
              matpis = recode(matpis, !!!recod_piso),
              servagua = recode(servagua, `1` = 1,  .default = 0), # preserva el NA 
              servbaño = recode(servbaño, `1` = 1,  .default = 0),
              servluz = recode(servluz, `1` = 1,  .default = 0),
              across(starts_with(c("activo", "oserv")), ~recode(.x, `1`= 0, `2`= 1)))
      )

# (2) imputamos   ----
items_ise <- ise_recod[[1]]$cod

# por lo menos 2/3 de los datos completos
n_imp <- length(items_ise) - length(items_ise)*2/3 + 1

rowSumsNA <- function(x) rowSums(is.na(x)) #contar NA en las filas

bd_para_imputar_l <- lista_iseb_r %>%
  map(~.x %>%
        mutate(nas = rowSumsNA(across(all_of(items_ise)))) %>% #contamos NA
        filter(nas <= n_imp) %>% #nas con menos de 2/3 
        select(-nas))

bd_para_imputar_l <- bd_para_imputar_l %>%
  map(~mutate(.x, across(starts_with(c("mat", "serv", "activo", "oserv")), as.factor)))

# imputacion *********************************************
library(mice)

# verificamos: 
lapply(bd_para_imputar_l, function(x) sapply(x[items_ise], class))

# loop para imputar
nom <- names(bd_para_imputar_l)
bd_complete_l <- map(lista, function(x) NULL)


for(i in 1:length(nom)){ #i=1
  
  bd_para_imputar <- bd_para_imputar_l[[nom[i]]]
  
  dry <- mice(bd_para_imputar, maxit = 0, print = F)
  pred <- dry$predictorMatrix
  meth <- dry$method
  
  #excluir de la prediccion : 
  pred[, c("ID", 'cod_mod7', 'anexo', "cor_minedu",  'cor_est')] <- 0 
  
  meth #ver que tienen el metodo correcto, sino editamos
  vise <- vars_ise[[nom[i]]]
  
  #meth[vise[1:2]] <- "pmm" # predictive mean matching: años de educacion 
  #meth[vise[3:5]] <- "polr" # proportional odds model: materiales de la vivienda 
  #meth[vise[6:29]] <- "logreg" #logistic regression: servicios basicos y activos 
  
  inicio <- Sys.time()
  temp.data <- mice(data = bd_para_imputar, method = meth, predictorMatrix = pred, m = 1, seed = 700, maxit = 1)
  final <- Sys.time()
  final - inicio 
  
  bd_complete_l[[i]] <- tibble(complete(temp.data, 1))

}


#sapply(bd_complete[items_ise], function(x) mean(is.na(x)))
#sapply(bd_complete[items_ise], function(x) sum(is.na(x)))

# fin imputacion ******************************************

# guardamos ! 
#rio::export_list(bd_complete_l, here("01-data", "03-intermedias", "01b-imputadas-ise", paste0(names(lista), "_ise.rds")))
rio::export(bd_complete_l[[1]], here("01-data", "03-intermedias", "01b-imputadas-ise", paste0(names(bd_complete_l[1]), "_ise.rds")))



