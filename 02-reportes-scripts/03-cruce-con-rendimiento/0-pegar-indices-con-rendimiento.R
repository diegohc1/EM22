# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# # Pegar escalas con rendimiento  
# *****************************************************************************************************************************

rm(list = ls())

library(tidyverse)
library(here)

# bases de datos
lista_rend = rio::import_list(Sys.glob(here("01-data", "05-bd-rendimiento", "*.sav")), setclass = "tibble") 
names(lista_rend[[1]])

lista_f = rio::import_list(Sys.glob(here("01-data", "04-para-el-analisis", "*.sav"))) %>% map(rio::factorize)

# quedemos con las variables 
lista_f1 <- lista_f %>%
  map(~select(.x, cod_mod7, starts_with("cor"), 
              starts_with(c("p0", "p1", "p2", "p3")), 
              starts_with(c("EST", "FAM", "DOC", "DIR", "ise"))))

# las llaves son:
# estudiante -> cor_minedu cor_est 
# familia -> cor_minedu cor_est 
# docente -> cor_minedu
# director -> cod_mod7

# 2S
s2 <- keep(lista_f1, str_detect(names(lista_f1), "2S"))
names(s2)
llaves <- list(dir = "cod_mod7", docc = "cor_minedu", docct = "cor_minedu",  
               est = c("cor_minedu", "cor_est"), est = c("cor_minedu", "cor_est"))

rend2sl <- map2(s2, llaves, ~left_join(lista_rend[[1]], .x, by = .y))

nrow(rend2sl$EM2022_2Sestudiante_EBRD1)
save(rend2sl, file = here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))







