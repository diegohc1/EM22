# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Genera un excel/rdata con el rendimiento promedio por item 
# *****************************************************************************************************************************

# algo de dificultad: para cada item, para cada rend, para cada actor, para cada grado 

rm(list = ls())
library(tidyverse)
library(here)

# tomas las variables asociadas al area (rend)
f1 <- function(data, area){
  
  if(area == "lectura"){
    p <- "mate|CN|MA"
    w <- "Peso_lectura"
  }else if (area == "mate"){
    p <- "CT|lectura|CN"
    w <- "Peso_mate"
  }else if (area == "ciencia"){
    p <- "CT|lectura|mate|MA"
    w <- "Peso_CN"
  }
  
  nom1 <- names(data)  
  temp <- nom1[!str_detect(nom1, p)]
  data1 <- data[temp]
  names(data1)[names(data1) == w] <- "Peso_final"
  return(data1)
  
}
source(here("00-insumos", "0-funciones-apoyo.R"))

# bases
# base ffaa pegado con rendimiento 
load(here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

#mismas bases en miau y lista
matriz1 <- matriz %>% filter(Concatena1 %in% names(rend2sl))
matriz1 <- filter(matriz1, TipoV %in% c("Categorico1", "Categorico2"))
rend2sl <- keep(rend2sl, names(rend2sl) %in% matriz1$Concatena1) 

matriz_lista <- split(matriz1, matriz1$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(rend2sl)] #mismo orden

lista_vars <- matriz_lista %>% map(~.x[["cod_preg"]])
matriz_vs_lista(rend2sl, lista_vars)

matriz_pegar <- bind_rows(matriz_lista) %>%
  select(Concatena1, Instrumento, cod_gen, cod_preg, Pregunta, Enunciado, TipoV, OpcionL) %>%
  drop_na(cod_gen)

# acomodaciones: 

# generamos listas segun rendimiento 
# por ejemplo lec2s tiene todas las bases de 2S con el rend en lectura

# por ejemplo, f1(..., "lectura") quita todo lo relacionado con mate o ciencia
# en realidad no es necesario quitarlo, pero te ordena, a miras de que no itere lo que no deberia
# tambien, a todos los pesos (Peso_lectura) los renombro "peso_final"

# 2S 
lec2s <- rend2sl %>% map(~f1(.x, "lectura")) 
mat2s <- rend2sl %>% map(~f1(.x, "mate")) 
cyt2s <- rend2sl %>% map(~f1(.x, "ciencia")) 

# 4P 


# 2P 


# ******

# 

# juntamos todas las listas
lista_rend_ffaa <- tibble::lst(lec2s, mat2s, cyt2s)


# como lista_rend_ffaa es una lista de listas, 
# lista_vars_l tambien sera una lista de listas
# la idea es que lista_vars_l$lec2s sea insumo de lista_rend_ffaa$lec2s 
# y asi sucesivamente... 
# para un map2(lista_vars_l, lista_rend_ffaa)

lista_vars_l <- list(
  lec2s = keep(lista_vars, str_detect(names(lista_vars), "2S")),
  mat2s = keep(lista_vars, str_detect(names(lista_vars), "2S")),
  cyt2s = keep(lista_vars, str_detect(names(lista_vars), "2S"))
  # 
)

#colocarle el nombre a cada elemento 
lista_vars_l <- map_depth(lista_vars_l, 2, ~setNames(.x, nm = .x)) 

# vector con todos los "rendimientos" 
vrend <- c("M500_EM_2S_2022_CT", "M500_EM_2S_2022_MA", "M500_EM_2S_2022_CN")

# rendimiento promedio para cada item
# descriptivo general ----

tabla_general <- map(lista_vars_l, function(x) NULL) 
for(i in 1:length(vrend)){
  
  tabla_general[[i]] <- 
    # iteramos para cada rendimiento (i), base (b) y variable (v)
    map2(lista_rend_ffaa[[i]], lista_vars_l[[i]], function(b, v)
      map(v, ~b %>% # para cada b, agrupa por todos los v
            group_by(.data[[.x]]) %>% #  
            drop_na(Peso_final) %>%
            factorito::mean_prop_grupo(vrend[[i]], w = "Peso_final") %>%
            rename(opcion = 1)) %>%
        bind_rows(.id = "cod_preg")) %>%
    bind_rows(.id = "Concatena1")
}


tabla_general1 <- bind_rows(tabla_general, .id = "rend")
tabla_general1 <- mutate(tabla_general1, estrato = "General")

#le pegamos la info del MIAU 🐈
tabla_general2 <- left_join(tabla_general1, matriz_pegar, by = c("Concatena1", "cod_preg"))
names(tabla_general2)

tabla_general3 <- tabla_general2 %>%
  select(Instrumento, Concatena1, rend, estrato, cod_gen, 
         cod_preg, Pregunta, Enunciado, opcion, n, prop, media, TipoV, OpcionL) %>%
  mutate(media = round(media, 0))

# en .rdata
save(tabla_general3, file = here("02-reportes-scripts", "03-cruce-con-rendimiento", "01-rendimiento-por-item", "01-rend-por-item.Rdata"))

# en .xlsx
tabla_general3_excel <- tabla_general3 %>%
  select(-TipoV, -OpcionL, -n) %>%
  split(.$Concatena1) %>%
  map(~pivot_wider(.x, names_from = rend, values_from = c(prop, media)))

rio::export(tabla_general3_excel, here("02-reportes-scripts", "03-cruce-con-rendimiento", "01-rendimiento-por-item", "01-rend-por-item.xlsx"))
# fin

# *******************************************************************************

# descriptivos segun gestion ---- 

tabla_estrato <- map(lista_vars_l, function(x) NULL) 
for(i in 1:length(vrend)){ # i=1
  
  tabla_estrato[[i]] <- 
    # iteramos para cada rendimiento (i), base (b) y variable (v)
    map2(lista_rend_ffaa[[i]], lista_vars_l[[i]], function(b, v)
      map(v, ~b %>% # para cada b, agrupa por todos los v
            group_by(gestion, .data[[.x]]) %>% #  
            drop_na(Peso_final) %>%
            factorito::mean_prop_grupo(vrend[[i]], w = "Peso_final") %>%
            rename(opcion = 2)) %>%
        bind_rows(.id = "cod_preg")) %>%
    bind_rows(.id = "Concatena1")
}

tabla_estrato1 <- bind_rows(tabla_estrato, .id = "rend")


# descriptivos segun sexo [solo estudiante]  ---- 

lista_rend_ffaa_sex <- map(lista_rend_ffaa, ~keep(.x, str_detect(names(.x), "familia|estudiante"))) %>% compact()
names(lista_rend_ffaa_sex); map(lista_rend_ffaa_sex, names)
map_depth(lista_rend_ffaa_sex, 2, names)

lista_vars_lsex <- map(lista_vars_l, ~keep(.x, str_detect(names(.x), "familia|estudiante"))) %>% compact()
names(lista_vars_lsex); map(lista_vars_lsex, names)
#vars_medida_sex <- vars_medida[1:4] #por ahora no 4P

tabla_sex <- map(lista_vars_lsex, function(x) NULL) 
for(i in 1:length(vrend)){ # i=1
  
  tabla_sex[[i]] <- 
    # iteramos para cada rendimiento (i), base (b) y variable (v)
    map2(lista_rend_ffaa_sex[[i]], lista_vars_lsex[[i]], function(b, v)
      map(v, ~b %>% # para cada b, agrupa por todos los v
            group_by(sexo, .data[[.x]]) %>% #  
            drop_na(Peso_final) %>%
            factorito::mean_prop_grupo(vrend[[i]], w = "Peso_final") %>%
            rename(opcion = 2)) %>%
        bind_rows(.id = "cod_preg")) %>%
    bind_rows(.id = "Concatena1")
}

tabla_sex1 <- bind_rows(tabla_sex, .id = "rend")

#le pegamos la info
tabla_sex2 <- left_join(tabla_sex1, matriz_pegar, by = c("Concatena1", "cod_preg"))

tabla_sex3 <- tabla_sex2 %>%
  select(Instrumento, Concatena1, rend, sexo, cod_gen, 
         cod_preg, Pregunta, Enunciado, opcion, n, prop, media, TipoV, OpcionL) %>%
  mutate(media = round(media, 0))

# en .rdata
save(tabla_sex3, file = here("02-reportes-scripts", "03-cruce-con-rendimiento", "01-rendimiento-por-item", "01-rend-por-item-sex.Rdata"))

# en .xlsx
tabla_sex3_excel <- tabla_sex3 %>%
  select(-TipoV, -OpcionL, -n) %>%
  split(.$Concatena1) %>%
  map(~pivot_wider(.x, names_from = rend, values_from = c(prop, media)))

rio::export(tabla_sex3_excel, here("02-reportes-scripts", "03-cruce-con-rendimiento", "01-rendimiento-por-item", "01-rend-por-item-sex.xlsx"))

# fin! espero! 

