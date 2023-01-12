# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# # CFA y PCA RECURSIVO 
# *****************************************************************************************************************************

# para inspeccionar indicadores de ajuste y cargas!


rm(list = ls())
library(tidyverse)
library(here)
library(lavaan)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))

# cargamos y acomodamos datos **********************
lista = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "01a-imputadas", "*.rds")))

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

matriz_cfa <- matriz %>%
  filter(Analisis2 %in% c("CFA", "PCA")) %>%
  drop_na(Cod_indice) %>%
  select(Concatena1, Constructo = Constructo_indicador,sub_escala, starts_with("Cod"), Enunciado, Analisis2) 

#mismas bases en miau y lista
matriz_cfa <- matriz_cfa %>% filter(Concatena1 %in% names(lista))
lista <- keep(lista, names(lista) %in% matriz_cfa$Concatena1)

matriz_lista <- split(matriz_cfa, matriz_cfa$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

#comprobemos si estan iguales
matriz_vs_lista(matriz_lista, lista)

nom <- names(matriz_lista) #para filtrar

#para guardar 
indic_cfa <- NULL; cargas_cfa <- NULL
indic_pca <- NULL; cargas_pca <- NULL
prec <- map(lista, function(x) NULL)

inicio <- Sys.time()
for(i in 1:length(nom)){ #i=5
  
  #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
  matriz_i <- select(matriz_lista[[nom[i]]], starts_with(c("cod", "Cod")), Analisis2, Enunciado, Constructo, sub_escala)
  vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i
  tipo <- distinct(matriz_i, Cod_indice, .keep_all = T)$Analisis2 #para identifcar pca o cfa despues
  
  bd <- lista[[nom[i]]] #tomamos la base i
  
  for(j in 1:length(vcod_indice)){ #j=4
    
    #Rutina para la escala 'j' de la base 'i'
    escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
    preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
    enunciado <- escala_j[c("cod_preg", "Enunciado")] #enunciados de la escala
    constructo_j <- unique(escala_j$Constructo)
    cod_constructo <- unique(escala_j$Cod_indice)
    bd1 <- bd[preg$cod_preg]
    bd2 <- drop_na(bd1) # listo para correr! 
    
    
    #prec[[i]][[vcod_indice[j]]]  <- tryCatch({ # para ver donde estan los warnings 
    
    # Corremos el modelo según PCA o CFA
    if(tipo[j] == "PCA"){ 
       resultados1 <- pca_recursivo(bd2, recursivo = TRUE, puntajes = FALSE)
    }else{ # CFA
      mod <- acomoda_string_lavaan(preg)
      resultados1 <- cfa_recursivo(bd2, model_lavaan = mod, recursivo = TRUE, puntajes = FALSE)
    }
    
    #  }, warning = function(w) print(w$message)
    #)
    
    
    # Ordenamos la información
    if(tipo[j] == "PCA"){ 
      
      #PCA :::::::::::::::::::::::::
      
      if(length(resultados1) == 2){ # si tiene sugeridos
        
        para_pegar_cargas_pca <- resultados1$cargas %>%
          left_join(enunciado, ., by = c("cod_preg" = "Item")) %>%
          bind_cols(cuestionario = nom[[i]], cod_constructo = cod_constructo, constructo = constructo_j, .)
        
        para_pegar_indic_pca <- bind_cols(cuestionario = nom[[i]], cod_constructo = cod_constructo, 
                                          constructo = constructo_j, resultados1$indicadores)
        
      }else{ #si no tiene sugeridos
        
       para_pegar_cargas_pca <- resultados1$cargas %>%
          select(Item, Cargas.inicial = Cargas) %>%
          mutate(Cargas.sugerido = NA) %>% 
          left_join(enunciado, ., by = c("cod_preg" = "Item")) %>%
          bind_cols(cuestionario = nom[[i]], cod_constructo = cod_constructo, constructo = constructo_j, .)
        
       para_pegar_indic_pca <- bind_cols(cuestionario = nom[[i]], cod_constructo = cod_constructo, 
                                         constructo = constructo_j, pca_inicial = resultados1$indicadores, pca_sugerido = NA)
      }
       
    cargas_pca <- bind_rows(cargas_pca, para_pegar_cargas_pca) #generamos la tabla
    indic_pca <- bind_rows(indic_pca, para_pegar_indic_pca) #generamos la tabla
  
    # CFA :::::::::::::::::::::::::
    
    }else{ 
      
      if(length(resultados1) == 2){ # si tiene sugeridos
        
        para_pegar_cargas_cfa <- resultados1$cargas %>%
          left_join(enunciado, ., by = c("cod_preg" = "Item")) %>%
          bind_cols(cuestionario = nom[[i]], constructo = constructo_j[1], .) %>%
          select(cuestionario, cod_constructo = Escala, constructo, cod_preg, Enunciado, Est.inicial, Est.sugerido)
        
        para_pegar_indic_cfa <- resultados1$indicadores %>%
          bind_cols(cuestionario = nom[[i]], cod_constructo = cod_constructo, constructo = constructo_j, .)
        
        }else{ #si no tiene sugeridos
          
          para_pegar_cargas_cfa <-  resultados1$cargas %>%
            select(Escala, Item, Est.inicial = Est) %>%
            mutate(Est.sugerido = NA) %>%
            left_join(enunciado, ., by = c("cod_preg" = "Item")) %>%
            bind_cols(cuestionario = nom[[i]], cod_constructo = cod_constructo, constructo = constructo_j[1], .)
            
          para_pegar_indic_cfa <- resultados1$indicadores %>%
            select(Indicadores, Valores.inicial = Valores) %>%
            mutate(Valores.sugerido = NA) %>%
            bind_cols(cuestionario = nom[[i]], cod_constructo = cod_constructo, constructo = constructo_j[1], .)
        
      }
      
      cargas_cfa <- bind_rows(cargas_cfa, para_pegar_cargas_cfa) #generamos la tabla
      indic_cfa <- bind_rows(indic_cfa, para_pegar_indic_cfa) #generamos la tabla
    
    }
    
  }
  
}
final <- Sys.time()
final - inicio     

# para ver donde estan los warnings ! 
flatten(prec) %>% View()


#rr <- here("2-psicometrico", "1-inspeccion-cfa")
#ifelse(!dir.exists(rr), dir.create(rr), FALSE)


# acomodemos un poco para ver mejor en el excel
ba <- c("cfi", "tli", "srmr", "rmsea")
ba2 <- c(paste0(ba, ".inicial"), paste0(ba, ".sugerido"))

indic_cfa1 <- indic_cfa %>%
  mutate(across(starts_with("Valores"), round, 3)) %>%
  rename(inicial = Valores.inicial, sugerido = Valores.sugerido) %>%
  pivot_wider(names_from = Indicadores, values_from = c(inicial, sugerido), names_glue = "{Indicadores}_{.value}") 


rio::export(list("CFA_indicadores de ajuste" = indic_cfa1,
            "CFA_cargas factoriales" = cargas_cfa,
            "PCA_var_explicada" = indic_pca, 
            "PCA_cargas" = cargas_pca),
       here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "01-cfa-pca-inspeccion.xlsx"))
      


