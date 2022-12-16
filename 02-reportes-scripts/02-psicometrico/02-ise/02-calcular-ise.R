rm(list = ls())

# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Indice socioeconomico (ISE) -> calcular
# *****************************************************************************************************************************

library(tidyverse)
library(here)
library(rio)
library(googlesheets4)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
as.numeric.factor <- function(x) as.numeric(levels(x))[x]

#bases
lista = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "01b-imputadas-ise", "*.rds"))) 

# Acomodaciones finales para el PCA
lista2 <- lista %>%
  map(
    ~mutate(.x, across(where(is.factor), as.numeric.factor), #todo a numerico para el pca
            maxedu = reduce(select(., c(edum, edup)), pmax)) #max educación padre y madre
  )


# Aplicamos PCA para cada base
for(i in 1:length(lista2)){ #i=1
  
  bd <- lista2[[i]]
  
  bd_list <- list(
    materiales_vivienda = select(bd, starts_with("mat")),
    servicios_basicos = select(bd, starts_with("serv")),
    activos = select(bd, starts_with("activo")),
    otros_serv = select(bd, starts_with("oserv"))
  )
  
  pca_uno <- map(bd_list, ~pca_umc_reporte(.x, "poly"))
  pca_uno <- map_df(pca_uno, 1)
  
  pca_dos <- bind_cols(bd, pca_uno) %>%
    select(maxedu, materiales_vivienda, servicios_basicos, activos, otros_serv) %>%
    pca_umc_reporte()
  
  bdfin <- bind_cols(bd, ise2S = pca_dos$puntajes)
  
}

hist(pca_dos$puntajes)

rio::export(bdfin, here("01-data", "03-intermedias", "02b-puntajes-ise",  "EM22_est2S_ise.rds"))
# rio::export(bdfin, here("01-data", "03-intermedias", "02b-puntajes-ise", paste0(names(bd_complete_l[1]), "_ise.rds")))

# #generar reporte en excel
# 
# f1 <- function(data, indicador){
#   data %>%
#     pivot_longer(everything(), names_to = "base", values_to = "varianza_explicada") %>%
#     mutate(indicador = indicador)
# }
# 
# var_ex <- bind_rows(
#   map_df(ins_ise, list("activos", 2)) %>% f1(., "activos"),
#   map_df(ins_ise, list("servicios", 2)) %>% f1(., "servicios"),
#   map_df(ins_ise, list("FAM_ISE", 2)) %>% f1(., "ise")
# )
# 
# cargas <- bind_rows(
#   map(ins_ise, list("activos", 3)) %>% 
#     umcR::pega_lista("base") %>% mutate(indicador = "activos"),
#   
#   map(ins_ise, list("servicios", 3)) %>% 
#     umcR::pega_lista("base") %>% mutate(indicador = "servicios"),
#   
#   map(ins_ise, list("FAM_ISE", 3)) %>% 
#     umcR::pega_lista("base") %>%  mutate(indicador = "ise")
# ) %>%
#   select(base, indicador, Item, Pesos, Cargas)
# 
# writexl::write_xlsx(list(var_ex, cargas), path = here("0. Scripts", "ise", "indicadores_ise_3S_integrado-v4.xlsx"))
# 
# 




