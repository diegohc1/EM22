rm(list = ls())

# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# EFA -> ¿dimensiones?
# *****************************************************************************************************************************

# para realizar analisis factorial exploratorio 
# ¿como se relacionan los items de las escalas? 

library(tidyverse)
library(here)
library(psych)
#install.packages("GPArotation")
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
source(here("00-insumos", "0-funciones-apoyo.R"))

# cargamos y acomodamos datos **********************
lista = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "01-imputadas", "*.rds")))

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

matriz_efa <- matriz %>%
  filter(!is.na(Cod_indice), Analisis1 == "EFA") %>%
  select(Concatena1, Constructo = Constructo_indicador, sub_escala, starts_with("Cod"), Enunciado) 

#mismas bases en miau y lista
matriz_efa <- filter(matriz_efa, Concatena1 %in% names(lista))
lista <- keep(lista, names(lista) %in% matriz_efa$Concatena1)

matriz_lista <- split(matriz_efa, matriz_efa$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(lista)] #mismo orden

#comprobemos si estan iguales
matriz_vs_lista(matriz_lista, lista)

nom <- names(matriz_lista) #para filtrar
cargas_efa <- NULL
indica_ajuste <- NULL
prec <- map(lista, function(x) NULL)

for(i in 1:length(nom)){ #i=2
  
  #Preparamos los insumos/variables para la rutina de la base/cuestionario 'i' #
  matriz_i <- select(matriz_lista[[nom[i]]], cod_gen, cod_preg, Cod_indice, Enunciado, Constructo)
  vcod_indice <- unique(matriz_i$Cod_indice) #escalas del cuestionario i
  bd <- lista[[nom[i]]] #tomamos la base i
  
  for(j in 1:length(vcod_indice)){ #j=1 #retire '1' porque no esta corriendo, luego vemos 
    
    #Rutina (puntajes e insumos RT) para la escala 'j' de la base 'i'
    escala_j <- filter(matriz_i, Cod_indice == vcod_indice[j])
    preg <- pull(escala_j, cod_preg) #preguntas de la escala
    enunciado <- pull(escala_j, Enunciado) #enunciados de la escala
    cons <- unique(escala_j$Constructo) #nombre constructo
    
    bd1 <- select(bd, all_of(preg)) #base con id, para pegar despues y descriptivos
    bd1 <- drop_na(bd1)
    
   #prec[[i]][[vcod_indice[j]]]  <- tryCatch({ # para ver donde estan los warnings 
    
    #aplicamos EFA, de 1 a 4 factores #**********************************
    
    #EFA
    fa_1_4 <- map(c(1:4), ~fa(bd1, nfactors = .x, cor = "poly", fm = "wls"))
    
    #VSS
    nfac <- unclass(vss(bd1, fm = "wls", plot = FALSE, cor = "poly"))
    
    #parallel analysis
    paralel <- fa.parallel(bd1, cor = "poly", fm = "wls", plot = FALSE)
    
    # }, warning = function(w) print(w$message),
    #   error = function(e) print(e$error)
    # )
    
    #indicadores de ajuste EFA y VSS, MAP
    indica_ajuste_pega <- data.frame(
      Base = nom[i], 
      constructo = cons,
      cod_escala = vcod_indice[j], 
      nfac = c(1:4),
      RMSEA = nfac$vss.stats$RMSEA[1:4],
      SRMS = nfac$vss.stats$SRMR[1:4],
      TLI = map_dbl(fa_1_4, "TLI"),
      VSS1 = nfac$cfit.1[1:4],
      VSS2 = nfac$cfit.2[1:4],
      MAP = nfac$map[1:4],
      PA_factores = paralel$nfact,
      PA_ncomp = paralel$ncomp
    )
    
    indica_ajuste <- bind_rows(indica_ajuste, indica_ajuste_pega) #generamos la tabla
    
    
    temp <- 
      map(fa_1_4, 
          ~as.data.frame(unclass(.x$loadings)) %>% rownames_to_column("items")) %>%
      reduce(left_join, by = "items")
    
    temp <- setNames(temp, c("cod_preg", '1.1', '2.1', '2.2', '3.1', '3.2', '3.3', '4.1','4.2', '4.3', '4.4')) 
    
    cargas_efa_pegar <- temp %>%
      mutate(Base = nom[i],
             Enunciado = enunciado, 
             cod_escala = vcod_indice[j],
             constructo = all_of(cons)[1]) %>%
      select(Base, constructo, cod_escala, Enunciado, cod_preg, everything())
    
    cargas_efa <- bind_rows(cargas_efa, cargas_efa_pegar) #generamos la tabla
    
  }
  
}



detalle <- data.frame(
  indicador = c("RMSEA (root mean squared error of approximation):", 
                "SRMR (standardized root mean square residual):", 
                "TLI (Tucker-Lewis index):",
                "VSS (very simple structure):", 
                "MAP (minimum average partial):",
                "PA (parallel analysis):",
                "Por último, pero no menos importante:"),
  descripción = c("Un menor valor indica un mejor ajuste. Valores mayores a .10 se considera un ajuste pobre",
                  "Un menor valor indica un mejor ajuste. Valores mayores a .10 se considera un ajuste pobre",
                  "Un valor mayor a .90 indica un ajuste razonable y mayor a .95 un buen ajuste",
                  "Un mayor valor indica una mejor representación de una estructura simple",
                  "Un menor valor indica menores correlaciones parciales despues de extraer el componente principal",
                  "Número de factores o componentes que sugiere el análisis paralelo",
                  "Interpretabilidad - las cargas, direcciones, signos... nos hacen sentido?")
)


indica_ajuste <- mutate(indica_ajuste, mutate(across(where(is.numeric), round, 4)))
cargas_efa <- mutate(cargas_efa, mutate(across(where(is.numeric), round, 4)))

rio::export(
  list(detalle = detalle, indicadores = indica_ajuste, cargas = cargas_efa),
  here("02-reportes-scripts", "02-psicometrico", "00-efa", "00-efa-indicadores.xlsx")
  )

# reporte en pdf
r <- here("02-reportes-scripts", "02-psicometrico", "00-efa", "01-genera-reporte-pdf.Rmd")
rmarkdown::render(r, output_file = '01-efa-reporte', envir = new.env())



# warnings ? 
prec






