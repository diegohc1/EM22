# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Limpieza de bases de datos -> 
# *****************************************************************************************************************************
# 🛁
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
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))

# (A) Cargamos la información 

# bases de datos 
lista = rio::import_list(Sys.glob(here("01-data", "01-depuradas2", "*.sav")))

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

unique(matriz$Concatena1)
names(lista)
map(lista, names)

# nombres de las bases en el miau
nom <- c("EM2022_2SdocenteCOM_EBR",
         "EM2022_2SdocenteMAT_EBR",
         "EM2022_2SdocenteCYT_EBR",
         "EM2022_2Sdirector_EBR",
         "EM2022_2Sestudiante_EBRD2",
         "EM2022_2Sestudiante_EBRD1")


lista <- setNames(lista, nom)

# nos quedamos con la matriz asociada a las bases
matriz1 <- filter(matriz, Concatena1 %in% names(lista))
matriz1_l <- split(matriz1, matriz1$Concatena1)
matriz1_l <- matriz1_l[names(lista)]

# preguntas para recod
preg_recod <- map(matriz1_l, ~filter(.x, !is.na(OpcionN)) %>% pull(cod_preg))

# repetidos? 
# estudiante 
est <- lista$EM2022_2Sestudiante_EBRD1
est <- lista$EM2022_2Sestudiante_EBRD2
est <- mutate(est, id2 = paste0(cor_minedu, cor_est))
est <- select(est, cor_minedu, cor_est, id2)
dupli(est, "id2")

doc <- lista$EM2022_2SdocenteCOM_EBR
dupli(lista$EM2022_2SdocenteCYT_EBR, "cor-minedu")

lista$EM2022_2Sestudiante_EBRD1 <- distinct(lista$EM2022_2Sestudiante_EBRD1, cor_minedu, cor_est, .keep_all = TRUE)
lista$EM2022_2Sestudiante_EBRD2 <- distinct(lista$EM2022_2Sestudiante_EBRD2, cor_minedu, cor_est, .keep_all = TRUE)
lista$EM2022_2Sestudiante_EBRD1 <- drop_na(lista$EM2022_2Sestudiante_EBRD1, cor_minedu, cor_est)
lista$EM2022_2Sestudiante_EBRD2 <- drop_na(lista$EM2022_2Sestudiante_EBRD2, cor_minedu, cor_est)

map(lista, nrow)

# como estan? 
# map(1:length(lista), ~sapply(lista[[.x]][preg_recod[[.x]]], table))

# (1) recodificar letra a numero ----

# a <- c("", "A", "B", "C", "M", "X")
# para_recod <- setNames(1:16, LETTERS[1:16])
# para_recod <- para_recod[names(para_recod) != "M"]
# recode(a, !!!para_recod, .default = NA_integer_)

# letra a numero 
para_recod <- setNames(1:16, LETTERS[1:16])
para_recod <- para_recod[names(para_recod) != "M"]

lista2 <- map2(lista, preg_recod, 
               ~mutate(.x, across(all_of(.y), ~recode(.x, !!!para_recod, .default = NA_integer_)))
               )

# asegurar que respuestas numericas sean numeros ! 
preg_recod_num <- map(matriz1_l, ~filter(.x, TipoV == "Numerica") %>% pull(cod_preg))
preg_recod_num <- compact(preg_recod_num)

for(i in 1:length(preg_recod_num)){ #i=1
  nomnum <- names(preg_recod_num[i])
  pregnum <- preg_recod_num[[i]]
  lista2[[nomnum]] <- lista2[[nomnum]] %>% mutate(across(all_of(pregnum), as.numeric))
}


# (2) ajustar inconsistencias ----

matriz %>% 
  filter(!is.na(Inconsistencia)) %>%
  select(Concatena1, Inconsistencia)

# director ****
# no puedes tener más experiencia especifica que general 
lista2$EM2022_2Sdirector_EBR <- lista2$EM2022_2Sdirector_EBR %>%mutate(p06 = ifelse(p06 > p05, p05, p06))  

# lista2$EM2022_2Pdirector_EBR <- lista2$EM2022_2Pdirector_EBR %>% mutate(p05 = ifelse(p05 > p04, p04, p05))  

# si no tienes tit pedagogico, solo puedes marcar que 
lista2$EM2022_2Sdirector_EBR <- lista2$EM2022_2Sdirector_EBR %>% mutate(p04 = ifelse(p03 == 1, 1, p04))

# lista2$EM2022_2Pdirector_EBR <- lista2$EM2022_2SPdirector_EBR %>% mutate(p04 = ifelse(p03 == 1, 1, p04))
# 
# lista2$EM2022_6Pdirector_EBR <- lista2$EM2022_6Pdirector_EBR %>% mutate(p04 = ifelse(p03 == 1, 1, p04))

# docente ****
# si no has terminado tus estudios secundarios, no puedes tenet tit pedagogico 
lista2$EM2022_2SdocenteCOM_EBR <- lista2$EM2022_2SdocenteCOM_EBR %>% mutate(p04 = ifelse(p03 == 1, 1, p04))

lista2$EM2022_2SdocenteCYT_EBR <- lista2$EM2022_2SdocenteCYT_EBR %>% mutate(p04 = ifelse(p03 == 1, 1, p04))

# lista2$EM2022_4PdocenteMAT_EBR  <- lista2$EM2022_4PdocenteMAT_EBR %>% mutate(p05 = ifelse(p04 == 1, 1, p05))
# 
# lista2$EM2022_4PdocenteCOM_EBR <- lista2$EM2022_4PdocenteCOM_EBR %>% mutate(p05 = ifelse(p04 == 1, 1, p05))

# si no enseñas ciencia, para otra oportunidad 
vct <- c("p06_01", "p06_02", "p06_03", "p06_04", "p06_05", "p06_06")

lista2$EM2022_2SdocenteCYT_EBR <- lista2$EM2022_2SdocenteCYT_EBR %>%
  mutate(temp = ifelse(if_all(all_of(vct), ~.x == 1), 1, 0),
         temp2 = ifelse(temp == 1 & p06_07 == 2, 1, 0)) %>%
  filter(temp2 != 1) %>%
  select(-temp, -temp2)

           
# estudiante ****
# no puedes repetir si nunca lo has hecho
lista2$EM2022_2Sestudiante_EBRD1 <- lista2$EM2022_2Sestudiante_EBRD1 %>%
  mutate(p05 = ifelse(p04 == 1, 1, p05), p04 = ifelse(p05 == 1, 1, p04))


# (2.1) agregar columnas de conocimiento pedagogico ----
f1 <- function(p, c){ifelse(p == c, 1, 0)} # funcion de ayuda

conc <- unique(claves$Concatena1)
for(i in 1:2){# i=2
  cpreg <- filter(claves, Concatena1 == conc[[i]])$cod_preg
  correcta <- filter(claves, Concatena1 == conc[[i]])$correcta

  lista2[[conc[[i]]]] <- aa <- map2(cpreg, correcta, 
                                ~lista2[[conc[[i]]]] %>%
                                  mutate(!!sym(paste0(.x, "r")) := f1(.data[[.x]], .y))) %>%
    map2(.,  paste0(cpreg, "r"), ~select(.x, all_of(.y))) %>%
    reduce(bind_cols) %>%
    bind_cols(lista2[[conc[[i]]]], .)
  
}

# (3) recodificar numero a etiqueta ----
recfac_l <- matriz1_l %>%
  map2(preg_recod, ~filter(.x, cod_preg %in% .y)) %>%
  map(~mutate(.x, across(starts_with("Opcion"), ~str_split(.x, ";"))))

for(i in 1:length(lista2)){ #i=4
  for(j in 1:length(preg_recod[[i]])){ #j=34
    recfac_i <- recfac_l[[i]]
    var <- recfac_i$cod_preg[[j]] 
    etiq <- filter(recfac_i, cod_preg == var)
    lista2[[i]][[var]] <- factor(lista2[[i]][[var]], unlist(etiq$OpcionN), unlist(etiq$OpcionL))  
  }
}

#lista2[[2]] %>% View()



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

lista3 <- map2(lista2, labels_l, ~asigna_label(.x, .y$label, .y$cod_preg))
# lista3[[2]] %>% View()

# (5) ajustar otras columnas  ----



# (6) guardar las bases ! 
# agregamos ID 
# para pegar más facil despues! 
lista3 <- map(lista3, ~tibble::rowid_to_column(.x, "ID"))

#names(lista3)
export_list(lista3, here("01-data", "02-con-etiquetas", paste0(names(lista3), ".sav")))


