# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# # CFA y PCA 
# *****************************************************************************************************************************
# para generar insumos para el reporte y puntajes!

rm(list = ls())
library(tidyverse)
library(here)
library(rio)
library(lavaan)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))

#cargamos resultados descriptivos
load(file = here("02-reportes-scripts", "01-descriptivos", "01-descriptivos-ffaa.Rdata"))
tdesc <- tab_final2[which(tab_final2$estrato == "General"), ]
tdesc <- mutate(tdesc, prop = round(prop, 1)) #redondeamos

# Generar excel con los indicadores de ajuste y las cargas factoriales #

# cargamos y acomodamos datos **********************
lista = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "01-imputadas", "*.rds")))

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

matriz_cfa <- matriz %>%
  filter(Analisis2 %in% c("CFA", "PCA")) %>%
  drop_na(Cod_indice) %>%
  select(Concatena1, Constructo = Constructo_indicador, sub_escala, starts_with("Cod"), Enunciado, Analisis2) 

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

quieres_puntajes <- TRUE

# para seleccionar llaves
llavesl <- list(
  dir = c("ID", "cod_mod7", "anexo"), 
  doc = c("ID", "cod_mod7", "anexo", "cor_minedu", "dsc_seccion"), 
  fam = c("ID", "cod_mod7", "anexo", "cor_minedu", "dsc_seccion_imp", "cor_est")
)


{
  inicio <- Sys.time()
  for(i in 1:length(nom)){ #i=1
    
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
    
    for(j in 1:length(vcod_indice)){ #j=1
      
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
      
      # Corremos el modelo según PCA o CFA
      if(tipo[j] == "PCA"){ 
        resultados1 <- pca_recursivo(bd3, recursivo = FALSE, puntajes = quieres_puntajes)
      }else{ # CFA
        mod <- acomoda_string_lavaan(preg)
        resultados1 <- cfa_recursivo(bd3, model_lavaan = mod, recursivo = FALSE, puntajes = quieres_puntajes)
      }
      
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
# se demoró 23 min (!) ... pero con todas las escalas de la EM :O... 1 hora? 
# 24 min [segunda vez]
# 25 min tercera vez

m_constructos <- matriz_cfa %>%
  mutate(Cod_indice2 = ifelse(is.na(Cod_indice2), Cod_indice, Cod_indice2)) %>%
  distinct(Concatena1, Cod_indice2, .keep_all = TRUE) %>%
  select(Concatena1, Cod_indice, Cod_indice2, Constructo) %>%
  split(.$Concatena1)

# guardamos puntajes
if(quieres_puntajes != FALSE){
  
  # Ultimos toques:
  # colocarle label [metadata de la columna]
  lab <- map(m_constructos, ~pull(.x, 4))
  nomcol <- map(m_constructos, ~pull(.x, 3))
  puntajes_lista <- list(puntajes_lista, lab, nomcol) %>% pmap(~asigna_label(..1, ..2, ..3)) 
  
  # estandarizar escalas con M=0, SD=1
  scale2 <- function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  puntajes_lista <- map2(puntajes_lista, nomcol, ~mutate(.x, across(all_of(.y), ~scale2(.x))))
  
  rio::export_list(puntajes_lista, here("01-data", "03-intermedias", "02-puntajes-factoriales", paste0(names(lista), ".rds")))
}


# guardamos insumos 
save(ins, file = here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "02-info-escalas-para-reporte.Rdata")) #guardamos

matriz_cfa %>%
  distinct(Concatena1, Constructo, .keep_all = TRUE) %>%
  select(Concatena1, Constructo)

map(lista, nrow)


#rr <- here("1-data", "4-puntajes")
#ifelse(!dir.exists(rr), dir.create(rr), FALSE)


# m00 <- cfa(mod, data = bd3, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")
# 
# l <- parameterEstimates(m00) %>% 
#   filter(op == "=~") %>%
#   pull(est)
# 
# theta <- parameterEstimates(m00) %>% 
#   filter(op == "~~") %>%
#   pull(est)
# 
# sum_theta <- sum(diag(lavInspect(m00, "est")$theta))
# sum_l2 <- sum(lavInspect(m00, "est")$lambda)^2
# 
# 
# sum_l2/(sum_l2 + sum_theta)
# resultados1$confiabilidad
# psych::alpha(bd3)$total$raw_alpha
# 
# psych::omegaSem(bd3)





