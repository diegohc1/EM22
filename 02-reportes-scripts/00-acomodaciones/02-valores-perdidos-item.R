# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Limpieza de bases de datos -> 
# *****************************************************************************************************************************

# Este script realiza una serie de acomodaciones iniciales para el uso de las bases de datos de FFAA

# (1) recodificar letra a número 
# (2) ajustar inconsistencias 
# (3) recodificar numero a etiqueta
# (4) colocar labels (atributos a las columnas)
# (5) ajustar otras columnas (etiqueta gestion, estrato, etc)

rm(list = ls())
library(here)
library(rio)
library(tidyverse)

# (A) Cargamos la información 

# bases de datos 
lista = rio::import_list(Sys.glob(here("01-data", "01-depuradas", "*.sav")))

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

unique(matriz$Concatena1)
names(lista)

# nombres de las bases en el miau
nom <- c("EM2022_2SdocenteCOM_EBR", 
         "EM2022_2SdocenteMAT_EBR",
         "EM2022_2SdocenteCYT_EBR", 
         "EM2022_2Sdirector_EBR", 
         "EM2022_2Sestudiante_EBRD1",
         "EM2022_2Sestudiante_EBRD2")

lista <- setNames(lista, nom)

# nos quedamos con la matriz asociada a las bases
matriz1 <- filter(matriz, Concatena1 %in% names(lista))
matriz1_l <- split(matriz1, matriz1$Concatena1)
matriz1_l <- matriz1_l[names(lista)]

# preguntas para recod
preg_recod <- map(matriz1_l, ~filter(.x, !is.na(OpcionN)) %>% pull(cod_preg))

# como estan? 
map(1:length(lista), ~sapply(lista[[.x]][preg_recod[[.x]]], table))

# (1) recodificar letra a numero ----

# a <- c("", "A", "B", "C", "M", "X")
# para_recod <- setNames(1:16, LETTERS[1:16])
# para_recod <- para_recod[names(para_recod) != "M"]
# recode(a, !!!para_recod, .default = NA_integer_)

# letra a numero 
para_recod <- setNames(1:16, LETTERS[1:16])
para_recod <- para_recod[names(para_recod) != "M"]

lista_numero <- map2(lista, preg_recod, 
               ~mutate(.x, across(all_of(.y), ~recode(.x, !!!para_recod, .default = NA_integer_)))
               )

# lista_numero[[1]] %>% View()


# (2) ajustar inconsistencias ----

# (?)


# (3) recodificar numero a etiqueta ----
recfac_l <- matriz1_l %>%
  map2(preg_recod, ~filter(.x, cod_preg %in% .y)) %>%
  map(~mutate(.x, across(starts_with("Opcion"), ~str_split(.x, ";"))))

for(i in 1:length(lista_numero)){ #i=4
  for(j in 1:length(preg_recod[[i]])){ #j=34
    recfac_i <- recfac_l[[i]]
    var <- recfac_i$cod_preg[[j]] 
    etiq <- filter(recfac_i, cod_preg == var)
    lista_numero[[i]][[var]] <- factor(lista_numero[[i]][[var]], unlist(etiq$OpcionN), unlist(etiq$OpcionL))  
  }
}

#lista_numero[[6]] %>% View()

devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")

# (4) colocar labels (atributos a las columnas) ----
recodtipo <- setNames(c("C1", "C2", "RA", "RN"), c("Categorico1", "Categorico2", "Abierta", "Numerica"))

labels_l <- matriz1_l %>%
  map(
    ~mutate(.x, tipov2 = recode(TipoV, !!!recodtipo),
            label = ifelse(tipov2 == "C2", paste0(Pregunta, "_", Enunciado), Enunciado),
            label = paste0(tipov2, "_", label)) %>%
      select(cod_preg, label) 
  )

# quitar las preguntas sobre las secciones (en docente)
labels_l <- labels_l %>%
  map_if(str_detect(names(.), "docente"), ~filter(.x, !str_detect(cod_preg, "p01")))

lista3 <- map2(lista_numero, labels_l, ~asigna_label(.x, .y$label, .y$cod_preg))
lista3[[2]] %>% View()

# (5) ajustar otras columnas  ----


# (6) guardar las bases ! 

names(lista3)
export_list(lista3, here("01-data", "02-con-etiquetas", paste0(names(lista3), ".sav")))



