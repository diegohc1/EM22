
rm(list = ls())
library(tidyverse)
library(here)

# acomoda un poco
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


#*************************************************************************************
# excel rendimiento por item - en long 
# sabana de resultados!
#*************************************************************************************


# bases 
load(here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))
names(rend2sl)

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

#mismas bases en miau y lista
matriz1 <- matriz %>% filter(Concatena1 %in% names(rend2sl))
matriz1 <- filter(matriz1, TipoV %in% c("Categorico1", "Categorico2"))
rend2sl <- keep(rend2sl, names(rend2sl) %in% matriz1$Concatena1) 

matriz_lista <- split(matriz1, matriz1$Concatena1) #separamos matriz en listas
matriz_lista <- matriz_lista[names(rend2sl)] #mismo orden

lista_vars <- matriz_lista %>% map(~.x[["cod_preg"]])
matriz_vs_lista(lista, lista_vars)

matriz_pegar <- bind_rows(matriz_lista) %>%
  select(Concatena1, Instrumento, cod_gen, cod_preg, Pregunta, Enunciado, TipoV, OpcionL) %>%
  drop_na(cod_gen)

# generamos listas segun rendimiento 

# 2S 
lec2s <- rend2sl %>% map(~f1(.x, "lectura")) 
mat2s <- rend2sl %>% map(~f1(.x, "mate")) 
cyt2s <- rend2sl %>% map(~f1(.x, "ciencia")) 

# 4P 


# 2P 




# 

lista_rend_ffaa <- tibble::lst(lec2s, mat2s, cyt2s)

lista_vars 

lista_vars_l <- list(
  lec2s = keep(lista_vars, str_detect(names(lista_vars), "2S")),
  mat2s = keep(lista_vars, str_detect(names(lista_vars), "2S")),
  cyt2s = keep(lista_vars, str_detect(names(lista_vars), "2S"))
  # 
)

lista_vars <- lista_rend_ffaa %>%
  map_depth(2, ~map_df(.x, ~attr(.x, "label"))) %>%   
              gather() %>%
              filter(str_detect(value, "C1|C2")) %>%
              pull(key))

lista_rend_ffaa %>%
  

rr <- c("M500_EM_2S_2022_CT", "M500_EM_2S_2022_MA", "M500_EM_2S_2022_CN")

lista_vars <- map2(lista_vars, ~setNames(.x, nm = .x))



names(rend2sl$EM2022_2Sestudiante_EBRD1)
# M500_EM_2S_2022_CT Peso_lectura
# M500_EM_2S_2022_MA Peso_mate
# M500_EM_2S_2022_CN Peso_CN

rr <- c("M500_EM_2S_2022_CT", "M500_EM_2S_2022_MA", "M500_EM_2S_2022_CN")

bd <- rend2sl$EM2022_2Sestudiante_EBRD1
v <- lista_vars$EM2022_2Sestudiante_EBRD1 
v <- setNames(v, nm = v)

lista_vars <- map(lista_vars, ~setNames(.x, nm = .x))
lista_vars_l <- map_depth(lista_vars_l, 2, ~set_names(.x, .x)) #colocarle el nombre

names(rend2sl)
names(lista_vars)

# rendimiento promedio para cada item:

tabla_general <- map(lista_vars_l, function(x) NULL) 
for(i in 1:length(rr)){
  
  tabla_general[[i]] <- 
    # iteramos para cada base (b) y variable (v)
    map2(lista_rend_ffaa[[i]], lista_vars_l[[i]], function(b, v)
      map(v, ~b %>%
            group_by(.data[[.x]]) %>%
            drop_na(Peso_final) %>%
            factorito::mean_prop_grupo(rr[[i]], w = "Peso_final") %>%
            rename(opcion = 1)) %>%
        bind_rows(.id = "cod_preg")) %>%
    bind_rows(.id = "Concatena1")
  
}


tabla_general1 <- bind_rows(tabla_general, .id = "rend")
tabla_general1 <- mutate(tabla_general1, estrato = "General")

#le pegamos la info
tablafinal2 <- left_join(tabla_general1, matriz_pegar, by = c("Concatena1", "cod_preg"))
names(tablafinal2)

tablafinal3 <- tablafinal2 %>%
  select(Instrumento, Concatena1, rend, estrato, cod_gen, 
         cod_preg, Pregunta, Enunciado, opcion, n, prop, media, TipoV, OpcionL) %>%
  mutate(media = round(media, 0))

# en .rdata
save(tablafinal3, file = here("02-reportes-scripts", "03-cruce-con-rendimiento", "1-rendimiento-por-item", "01-rend-por-item.Rdata"))

#para excel
tt <- tablafinal3 %>%
  select(-TipoV, -OpcionL, -n) %>%
  split(.$Concatena1) %>%
  map(~pivot_wider(.x, names_from = rend, values_from = c(prop, media)))

rio::export(tt, here("02-reportes-scripts", "03-cruce-con-rendimiento", "1-rendimiento-por-item", "01-rend-por-item.xlsx"))



tab1 <- map2(rend2sl, lista_vars, function(b, v)
  map(v, ~b %>%
        group_by(.data[[.x]]) %>%
        drop_na(.data[[ww[1]]]) %>%
        factorito::mean_prop_grupo("M500_EM_2S_2022_CT", w = "Peso_final") %>%
        rename(opcion = 1)) %>%
    bind_rows(.id = "var")) %>%
  bind_rows(.id = "Concatena")






#bases
load(here("1-bases", "2-rendimiento", "02-pegado-con-ffaa", "bases_pegadas.Rda"))

matriz <- read_sheet("https://docs.google.com/spreadsheets/d/1Ur2phcc84D72tlUw44nhnOYed48BMb2cwyw3jwJzntc/edit#gid=341416768")

bases_ffaa <- map(lista_rend_ffaa, names) %>% 
  flatten() %>%
  unlist() %>%
  unique()

#mismas bases en miau y lista
matriz1 <- matriz %>% filter(Concatena1 %in% bases_ffaa)
matriz1 <- filter(matriz1, TipoV %in% c("Categorico1", "Categorico2"))

matriz_lista <- split(matriz1, matriz1$Concatena1) #separamos matriz en listas

matriz_pegar <- bind_rows(matriz_lista) %>%
  select(Concatena1, Instrumento, cod_gen, cod_preg, Pregunta, Enunciado, TipoV, OpcionL)

# ordenamos los parametros -----
lista_vars <- lista_rend_ffaa %>%
  map_depth(2, ~map_df(.x, ~attr(.x, "label")) %>%   
              gather() %>%
              filter(str_detect(value, "C1|C2")) %>%
              pull(key))

lista_vars <- map_depth(lista_vars, 2, ~set_names(.x, .x)) #colocarle el nombre
names(lista_vars)

vars_medida <- c("M500_EVA_2S_2021_CT", "M500_EVA_2S_2021_MA",  "M500_EVA_6P_2021_CT", 
                 "M500_EVA_6P_2021_MA", "M500_EVA_4P_2021_CT", "M500_EVA_4P_2021_MA")

lista_rend_ffaa$s2lec$EVA2021_2Sdocente_EBRG1 %>% View()

#descriptivos general ----

#probemos con un loop
tabla_general <- map(lista_vars, function(x) NULL) 
for(i in 1:length(vars_medida)){
  
  tabla_general[[i]] <- 
    map2(lista_vars[[i]], lista_rend_ffaa[[i]], function(v, b) 
      map(v, ~b %>%
            drop_na(.data[[.x]]) %>% 
            group_by(.data[[.x]]) %>% 
            mean_prop_estrato(.data[[vars_medida[[i]]]], peso = peso_final) %>%
            rename(opcion = 1)
          ) %>% pega_lista("cod_preg")
      ) %>% pega_lista("Concatena1")
  
}

tabla_general1 <- pega_lista(tabla_general, "rendimiento")
tabla_general1 <- mutate(tabla_general1, estrato = "General")

#le pegamos la info
tablafinal2 <- left_join(tabla_general1, matriz_pegar, by = c("Concatena1", "cod_preg"))
names(tablafinal2)

tablafinal3 <- tablafinal2 %>%
  select(Instrumento, Concatena1, rendimiento, estrato, cod_gen, 
         cod_preg, Pregunta, Enunciado, opcion, n, freq, media, TipoV, OpcionL) %>%
  mutate(media = round(media, 0))

# en .rdata
save(tablafinal3, file = here("3-reportes", "3-cruces-rendimiento", "01-rendimiento-item-ffaa.Rdata"))

#para excel
tt <- tablafinal3 %>%
  select(-TipoV, -OpcionL, -n) %>%
  split(.$Concatena1) %>%
  map(~pivot_wider(.x, names_from = rendimiento, values_from = c(freq, media)))

writexl::write_xlsx(tt, here("3-reportes", "3-cruces-rendimiento", "01-rendimiento-item-ffaa2.xlsx"))

#****************************************************************************************************************

# descriptivos estrato [estatal urbano, estatal rural, no estatal] ----

tabla_estrato <- map(lista_vars, function(x) NULL) 
for(i in 1:length(vars_medida)){
  
  tabla_estrato[[i]] <- 
    map2(lista_vars[[i]], lista_rend_ffaa[[i]], function(v, b) 
      map(v, ~b %>%
            drop_na(.data[[.x]], estrato) %>% 
            group_by(.data[[.x]], estrato) %>% 
            mean_prop_estrato(.data[[vars_medida[[i]]]]) %>%
            rename(opcion = 1)
      ) %>% pega_lista("cod_preg")
    ) %>% pega_lista("Concatena1")
  
}

tabla_estrato1 <- pega_lista(tabla_estrato, "rendimiento")

#****************************************************************************************************************

# descriptivos sexo [solo estudiante] ----

lista_rend_ffaa_sex <- map(lista_rend_ffaa, ~keep(.x, str_detect(names(.x), "familia|estudiante"))) %>% compact()
names(lista_rend_ffaa_sex)
map(lista_rend_ffaa_sex, names)
map_depth(lista_rend_ffaa_sex, 2, names)
lista_vars_sex <- map(lista_vars, ~keep(.x, str_detect(names(.x), "familia|estudiante"))) %>% compact()
names(lista_vars_sex)
map(lista_vars_sex, names)
#vars_medida_sex <- vars_medida[1:4] #por ahora no 4P
names(lista_vars_sex)

tabla_sex <- map(lista_vars_sex, function(x) NULL) 
for(i in 1:length(vars_medida)){ #i=5
  
  tabla_sex[[i]] <- 
    map2(lista_vars_sex[[i]], lista_rend_ffaa_sex[[i]], function(v, b) 
      map(v, ~b %>%
            drop_na(.data[[.x]], sexo) %>% 
            group_by(.data[[.x]], sexo) %>% 
            mean_prop_estrato(.data[[vars_medida[[i]]]]) %>%
            rename(opcion = 1)
      ) %>% pega_lista("cod_preg")
    ) %>% pega_lista("Concatena1")
  
}


tabla_sex1 <- pega_lista(tabla_sex, "rendimiento")
#tabla_sex1 <- rename(tabla_sex1, estrato = SEXO)

#le pegamos la info
tablafinal2 <- left_join(tabla_sex1, matriz_pegar, by = c("Concatena1", "cod_preg"))
#names(tablafinal2)

tablafinal3 <- tablafinal2 %>%
  select(Instrumento, Concatena1, rendimiento, sexo, cod_gen, 
         cod_preg, Pregunta, Enunciado, opcion, n, freq, media, TipoV, OpcionL) %>%
  mutate(media = round(media, 0))

# en .rdata
save(tablafinal3, file = here("3-reportes", "3-cruces-rendimiento", "01-rendimiento-item-ffaa-sex.Rdata"))

#para excel
tt <- tablafinal3 %>%
  select(-TipoV, -OpcionL, -n) %>%
  split(.$Concatena1) %>%
  map(~pivot_wider(.x, names_from = rendimiento, values_from = c(freq, media)))

writexl::write_xlsx(tt, here("3-reportes", "3-cruces-rendimiento", "01-rendimiento-item-ffaa-sex.xlsx"))


#************************************************************************************************************************


# para una base
# bd <- lista_rend_ffaa$s2lec$EVA2021_2Sdirector_EBRG1
# 
# v <- map_df(bd, ~attr(.x, "label")) %>%
#   gather() %>%
#   filter(str_detect(value, "C1|C2")) %>%
#   pull(key)
# 
# names(v) <- v
# 
# aa <- map(v, ~bd %>%
#             drop_na(.data[[.x]]) %>%
#             group_by(.data[[.x]]) %>%
#             mean_prop_estrato(s2lectura) %>%
#             rename(opcion = 1)) %>%
#   pega_lista("var")
# 
# aa <- map(v, ~bd %>%
#             drop_na(.data[[.x]]) %>%
#             group_by(.data[[.x]], estrato) %>%
#             mean_prop_estrato(s2lectura) %>%
#             rename(opcion = 1)) %>%
#   pega_lista("var")
# 
# 
# # para varias bases con el mismo rendimiento
# 
# s2vars <- lista_rend_ffaa$s2lec %>%
#   map(~map_df(.x, ~attr(.x, "label")) %>%   
#         gather() %>%
#         filter(str_detect(value, "C1|C2")) %>%
#         pull(key))
# 
# s2vars <- map(s2vars, ~set_names(.x, .x)) #colocarle el nombre
# 
# bb <- lista_rend_ffaa$s2lec
# 
# tabla1 <- 
#   map2(s2vars, bb, function(v, b) 
#     map(v, ~b %>%
#           drop_na(.data[[.x]]) %>% 
#           group_by(.data[[.x]]) %>% 
#           mean_prop_estrato(s2lectura) %>%
#           rename(opcion = 1)
#     ) %>% pega_lista("cod_preg")
#   ) %>% pega_lista("Concatena1")
# 



