# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# # Pegar escalas con rendimiento  
# *****************************************************************************************************************************

rm(list = ls())

library(tidyverse)
library(here)

# bases de datos
lista_rend = rio::import_list(Sys.glob(here("01-data", "05-bd-rendimiento", "*.sav")), setclass = "tibble") %>% map(rio::factorize)
# names(lista_rend[[1]] %>% View())

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


# hay repetidos en estudiante! 
# por ahora filtremos esos casos 

lista_f1$EM2022_2Sestudiante_EBRD1 <- lista_f1$EM2022_2Sestudiante_EBRD1 %>% distinct(cor_minedu, cor_est, .keep_all = TRUE) 
names(lista_f1$EM2022_2Sestudiante_EBRD1)


# 2S ------
# ****************************************************************
s2 <- keep(lista_f1, str_detect(names(lista_f1), "2S"))
names(s2)
map(s2, names)

llaves <- list(dir = "cod_mod7", 
               docc = "cor_minedu", 
               docct = "cor_minedu",
               docm = "cor_minedu",
               est = c("cor_minedu", "cor_est"), 
               est = c("cor_minedu", "cor_est")
               )

rend2sl <- map2(s2, llaves, ~left_join(lista_rend[[1]], .x, by = .y))
map(rend2sl, names)

rend2sl <- rend2sl %>%
  map_if(str_detect(names(.), "dir"), ~select(.x, -cor_minedu.y) %>% rename(cor_minedu = cor_minedu.x))

rend2sl <- rend2sl %>%
  map_if(str_detect(names(.), "doc"), ~select(.x, -cod_mod7.y) %>% rename(cod_mod7 = cod_mod7.x))

rend2sl <- rend2sl %>%
  map_if(str_detect(names(.), "est"), ~select(.x, -cod_mod7.y) %>% rename(cod_mod7 = cod_mod7.x))

save(rend2sl, file = here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))

# solo indices ! 
names(rend2sl$EM2022_2Sdirector_EBR)
rend2sl$EM2022_2SdocenteCOM_EBR

dir <- select(rend2sl$EM2022_2Sdirector_EBR, 1:52, starts_with("DIR")) 
doc1 <- select(rend2sl$EM2022_2SdocenteCOM_EBR, cor_minedu, cor_est, starts_with("DOC")) 
doc2 <- select(rend2sl$EM2022_2SdocenteCYT_EBR, cor_minedu, cor_est, starts_with("DOC")) 
doc3 <- select(rend2sl$EM2022_2SdocenteMAT_EBR, cor_minedu, cor_est, starts_with("DOC")) 
est1 <- select(rend2sl$EM2022_2Sestudiante_EBRD1, cor_minedu, cor_est, starts_with("EST"), ise2S, isep2S, -Estrato) 
est2 <- select(rend2sl$EM2022_2Sestudiante_EBRD2, cor_minedu, cor_est, starts_with("EST"), -Estrato) 

temp <- list(dir, doc1, doc2, doc3, est1, est2) %>% reduce(left_join, by = c("cor_minedu" ,"cor_est"))
rio::export(temp, file = here("01-data", "06-ffaa-con-rendimiento", "rend2s_indices.sav"))



