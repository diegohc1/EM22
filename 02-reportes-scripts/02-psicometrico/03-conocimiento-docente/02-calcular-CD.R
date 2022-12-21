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

# 1. Docente de COMU 
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

# 1. Docente de MATE  
bdmat <- lista$EM2022_2SdocenteMAT_EBR_CD # base
matriz_cfa_mat <- matriz_cfa %>%
  filter(Cod_indice == "DOC2SLEC_CODIDR") %>%
  mutate(cod_preg = paste0(cod_preg, "r"))

mod <- acomoda_string_lavaan(matriz_cfa_com)

bdcom <- bdcom %>%
  mutate(across(where(is.factor), as.numeric.factor)) %>%
  drop_na()

m1 <- cfa(mod, data = bdcom, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
reporte_lavaan(m1, puntajes = FALSE)



#mismas bases en miau y lista
matriz_cfa <- matriz_cfa %>% filter(Concatena1 %in% names(lista))
lista <- keep(lista, names(lista) %in% matriz_cfa$Concatena1)

matriz_lista <- split(matriz_cfa, matriz_cfa$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

#comprobemos si estan iguales
matriz_vs_lista(matriz_lista, lista)

nom <- names(matriz_lista) #para filtrar

#para guardar 
ins <- map(lista, function(x) NULL) #para guardar los insumos del RT
puntajes <- map(lista, function(x) NULL) 
puntajes_lista <- NULL
prec <- map(lista, function(x) NULL)

quieres_puntajes <- FALSE

# para seleccionar llaves
llavesl <- list(
  dir = c("ID", "cod_mod7", "anexo"), 
  doc = c("ID", "cod_mod7", "anexo", "cor_minedu", "dsc_seccion"), 
  fam = c("ID", "cod_mod7", "anexo", "cor_minedu", "dsc_seccion_imp", "cor_est")
)


{
  inicio <- Sys.time()
  for(i in 1:length(nom)){ #i=2
    
    #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
    matriz_i <- select(matriz_lista[[nom[i]]], starts_with(c("cod", "Cod")), Analisis2, Enunciado, Constructo, sub_escala)
    vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i
    tipo <- distinct(matriz_i, Cod_indice, .keep_all = T)$Analisis2 #para identifcar pca o cfa despues
    
    bd <- lista[[nom[i]]] #tomamos la base i
    
    tdesci <- tdesc[which(tdesc$Concatena1 == nom[i]), ] #descriptivos
    
    # quedarnos con las llaves segun base
    if (str_detect(nom[i], "director")) {
      ll <- llavesl[[1]]
    } else if (str_detect(nom[i], "docente")) {
      ll <- llavesl[[2]]
    } else if (str_detect(nom[i], "familia|estudiante")) {
      ll <- llavesl[[3]]
    }
    
    for(j in 1:length(vcod_indice)){ #j=3
      
      #Rutina para la escala 'j' de la base 'i'
      escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
      preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
      enunciado <- escala_j[c("cod_preg", "Enunciado")] #enunciados de la escala
      constructo_j <- unique(escala_j$Constructo)
      cod_constructo <- unique(escala_j$Cod_indice)
      #llave <- c("ID")
      bd1 <- bd[c(ll, preg$cod_preg)] #base con id para pegar los puntajes a la base
      bd2 <- drop_na(bd1)
      bd3 <- bd2[preg$cod_preg] 
      
      prec[[i]][[j]]  <- tryCatch({ # para ver donde estan los warnings ! 
        
        # Corremos el modelo según PCA o CFA
        if(tipo[j] == "PCA"){ 
          resultados1 <- pca_recursivo(bd3, recursivo = FALSE, puntajes = quieres_puntajes)
        }else{ # CFA
          mod <- acomoda_string_lavaan(preg)
          resultados1 <- cfa_recursivo(bd3, model_lavaan = mod, recursivo = FALSE, puntajes = quieres_puntajes)
        }
      }, warning = function(w) print(w$message)
      )
      
      if(quieres_puntajes != FALSE){
        if(tipo[j] == "PCA"){ #nombre a la columna generada con PCA
          resultados1$puntajes <- setNames(as.data.frame(resultados1$puntajes), unique(preg$Cod_indice))        
        }
        
        #guardamos los puntajes
        bd3_id_con_puntajes <- bind_cols(bd2["ID"], resultados1$puntajes)
        puntajes[[i]][[j]] <- bd3_id_con_puntajes
        
        # juntamos todos los datos imputados
        puntajes_lista[[i]] <-  reduce(puntajes[[i]], full_join, by = "ID")
        puntajes_lista[[i]] <- left_join(bd[ll], puntajes_lista[[i]], by = "ID")
        
        
      }
      
      #tabla de descriptivos ********
      tabla_desc <- tdesci[which(tdesci$cod_preg %in% preg$cod_preg), ] %>%
        .[c("cod_preg", "Enunciado", "opcion", "prop")] %>%
        pivot_wider(names_from = opcion, values_from = prop)
      
      #guardamos los insumos 
      ins[[i]][[vcod_indice[j]]] <- list(tabla_desc, 
                                         resultados1$cargas, 
                                         resultados1$indicadores, 
                                         resultados1$confiabilidad)
      
    }
    
  }
  final <- Sys.time()
  final - inicio 
}

