# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Imputar bases de datos  
# *****************************************************************************************************************************

# Imputar preguntas que conforman escalas/indices 
# Genera un reporte de missing de las escalas 
# Inspeccion de las correlaciones 


rm(list = ls())
library(here)
library(rio)
library(tidyverse)
library(mice)
source(here("00-insumos", "0-funciones-apoyo.R"))

# bases de datos 
lista = rio::import_list(Sys.glob(here("01-data", "02-con-etiquetas", "*.sav"))) 

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

matriz_escalas <- matriz %>%
  filter(Analisis2 %in% c("CFA", "PCA")) %>%
  drop_na(Cod_indice) %>%
  select(Concatena1, Constructo = Constructo_indicador, sub_escala, 
         starts_with("Cod"), Enunciado, Analisis2, Invertir, OpcionE) 

#mismas bases en miau y lista
matriz_escalas <- matriz_escalas %>% filter(Concatena1%in% names(lista))
lista <- keep(lista, names(lista) %in% matriz_escalas$Concatena1)

matriz_lista <- split(matriz_escalas, matriz_escalas$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

matriz_vs_lista(matriz_lista, lista) #comprobemos si estan iguales

nom <- names(matriz_lista) #para filtrar

datos_imputados <- map(lista, function(x) NULL) 
datos_imputados_lista <- NULL
tabla_info_missing_pegar <- NULL
correlaciones_lista <- map(lista, function(x) NULL)
inspeccion_correlaciones_lista <- map(lista, function(x) NULL) 

# agregamos ID
lista <- map(lista, ~tibble::rowid_to_column(.x, "ID"))

llavesl <- list(
  dir = c("ID", "cod_mod7", "anexo"), 
  doc = c("ID", "cod_mod7", "anexo", "dsc_seccion"), 
  fam = c("ID", "cod_mod7", "anexo", "dsc_seccion_imp", "cor_est")
)

{
  inicio <- Sys.time()
  for(i in 1:length(nom)){ #i=4
    
    #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i'
    matriz_i <- matriz_lista[[nom[i]]]
    vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i
    
    bd <- lista[[nom[i]]] #tomamos la base i
    
    for(j in 1:length(vcod_indice)){ #j=1
      
      #Rutina para la escala 'j' de la base 'i'
      escala_j <- matriz_i[which(matriz_i$Cod_indice == vcod_indice[j]), ]
      preg <- escala_j[c("cod_preg", "Cod_indice", "Cod_indice2")] #id de la escala
      constructo_j <- unique(escala_j$Constructo)
      elim_opc <- unique(escala_j$OpcionE)
      cod_constructo <- unique(escala_j$Cod_indice)
      llave <- c("ID")
      
      bd1 <- bd[c(llave, preg$cod_preg)] #base con id para pegar los puntajes a la base
      
      # preparamos datos para generar tabla con informacion de missing
      bd1_b <- bd1 %>%
        mutate(sum_nas = rowSumsNA(across(all_of(preg$cod_preg))),
               porc_nas = sum_nas/length(preg$cod_preg),
               nas_completo = ifelse(porc_nas == 0, 1, 0),
               nas_alguno = ifelse(porc_nas > 0, 1, 0),
               nas_se_recupera = ifelse(porc_nas > 0 & porc_nas < 0.25, 1, 0),
               nas_missing = ifelse(porc_nas >= 0.25, 1, 0)
        )
      
      tabla_info_missing <- bd1_b %>%
        summarise(across(starts_with("nas"), ~round(sum(.x)/nrow(bd1_b)*100, 2))) %>%
        rename(Completos = 1, Incompletos = 2, Casos_imputados = 3, Porcentaje_missing = 4) %>%
        bind_cols(cod_indice = cod_constructo,
                  constructo = constructo_j,
                  Total = nrow(bd1_b), .)
      
      # preparamos para imputacion
      bd2 <- bd1 %>%
        #identificar observaciones con mas de 25% de missing y retirarlas
        filter(apply(.[preg$cod_preg], 1, function(x) mean(is.na(x))) < 0.25) %>%
        mutate(across(all_of(preg$cod_preg), as.numeric)) 
      
      #imputación **********
      pred <- mice(bd2, maxit = 0, print = F)$predictorMatrix #'falso' mice, para excluir id
      pred[, llave] <- 0 #excluir id de la prediccion
      
      mice_data <- mice(bd2, m = 1, maxit = 20, meth = 'pmm', predictorMatrix = pred, seed = 343, print = FALSE) #imputacion
      # m=1 porque solo usaremos la primera imputación.... 
      bd3 <- as_tibble(complete(mice_data, 1)) # bd3: datos imputados
      #***********
      
      #para invertir
      if(any(!is.na(escala_j$Invertir))){
        items_invertir <- filter(escala_j, Invertir == "Invertir")$cod_preg
        bd3 <- mutate(bd3, across(all_of(items_invertir), ~invertir(.x, max(select(bd3, -id2)))))
      }
      
      #para eliminar la opcion que no entra al modelo
      if(!is.na(elim_opc)){
        bd3 <- filter(bd3, across(all_of(preg$cod_preg), ~.x != elim_opc))
        bd3 <- mutate(bd3, across(all_of(preg$cod_preg), ~.x - 1))
      }
      
      # para inspeccionar correlaciones
      bd3_corr <- preg %>%
        mutate(Cod_indice2 = ifelse(is.na(Cod_indice2), Cod_indice, Cod_indice2)) %>%
        split(., .$Cod_indice2) %>%
        map(1) %>%
        map(~select(bd3, all_of(.x))) %>% #sub_escalas
        map(~psych::polychoric(.x)$rho) #correlacion policor
      
      bd3_inspec_corr <- map(bd3_corr, ~chequeo(.x))
      
      correlaciones_lista[[i]][[j]] <- bd3_corr 
      inspeccion_correlaciones_lista[[i]][[j]] <- bd3_inspec_corr
      #**
      
      datos_imputados[[i]][[j]] <- bd3
      tabla_info_missing_pegar <- bind_rows(tabla_info_missing_pegar, tabla_info_missing)
      
    }
    
    # quedarnos con las llaves segun base
    if (str_detect(nom[i], "director")) {
      ll <- llavesl[[1]]
    } else if (str_detect(nom[i], "docente")) {
      ll <- llavesl[[2]]
    } else if (str_detect(nom[i], "familia|estudiante")) {
      ll <- llavesl[[3]]
    }
    
    # juntamos todos los datos imputados
    datos_imputados_lista[[i]] <- datos_imputados[[i]] %>%
      reduce(full_join, by = llave) %>%
      left_join(lista[[i]][ll], ., by = llave)
  }
  
  final <- Sys.time()

}
final - inicio 


# guardamos!

# bases imputadas
rio::export_list(datos_imputados_lista, here("01-data", "03-intermedias", "01-imputadas", paste0(names(lista), ".rds")))

# reporte de missing de las escalas
export(tabla_info_missing_pegar, here("02-reportes-scripts", "00-acomodaciones", "03-valores-perdidos-indices.xlsx"))

# correlaciones 
corrs <- flatten(correlaciones_lista) %>% flatten()
save(corrs, file = here("02-reportes-scripts", "00-acomodaciones", "04-correlaciones-items.Rdata"))
r <- here("02-reportes-scripts", "00-acomodaciones", "04-corr-items-generar-pdf.Rmd")
rmarkdown::render(r, output_file = '04-correlaciones-items', envir = new.env())

# inspeccion de correlaciones
bd3_corr_check_b <- map(inspeccion_correlaciones_lista, flatten) %>%
  map(~bind_rows(.x, .id = "constructo")) %>%
  bind_rows(.id = "base")

export(bd3_corr_check_b, here("02-reportes-scripts", "00-acomodaciones", "05-inspeccion-corr.xlsx"))

