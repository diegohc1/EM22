rm(list = ls())

# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Indice socioeconomico (ISE) -> calcular
# *****************************************************************************************************************************

library(tidyverse)
library(here)
library(rio)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
as.numeric.factor <- function(x) as.numeric(levels(x))[x]

#bases
lista = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "01b-imputadas-ise", "*.rds"))) 
aa <- lista[[1]]

# quedarnos con LIMA 
# d <- Sys.glob(here("01-data", "02-con-etiquetas", "*.sav"))
# temp <- rio::import(d[5])
# temp <- select(temp, cod_mod7, provinciaX) %>% distinct(cod_mod7, .keep_all = TRUE)
# temp %>% count(provinciaX)

# lista[[1]] <- left_join(lista[[1]], temp, by = "cod_mod7")
# lista[[1]] <- filter(lista[[1]], !provinciaX %in% c("CANTA", "HUAROCHIRÍ"))

# Acomodaciones finales para el PCA
lista2 <- lista %>%
  map(
    ~mutate(.x, across(where(is.factor), as.numeric.factor), #todo a numerico para el pca
            maxedu = reduce(select(., c(edum, edup)), pmax), #max educación padre y madre
            activo14_15 = ifelse(activo14 == 1 | activo15 == 1, 1, 0)
            ) 
  )


# Aplicamos PCA para cada base
for(i in 1:length(lista2)){ #i=1
  
  bd <- lista2[[i]]
  
  bd_list <- list(
    materiales_vivienda = select(bd, starts_with("mat")),
    servicios_basicos = select(bd, starts_with("serv")),
    activos = select(bd, starts_with("activo"), -activo14, -activo15),
    otros_serv = select(bd, starts_with("oserv"))
  )
  
  pca_uno <- map(bd_list, ~pca_umc_reporte(.x, "poly"))
  pca_unop <- map_df(pca_uno, 1)
  
  pca_dos <- bind_cols(bd, pca_unop) %>%
    select(maxedu, materiales_vivienda, servicios_basicos, activos, otros_serv) %>%
    pca_umc_reporte()
  
  bdfin <- bind_cols(bd, ise2S = pca_dos$puntajes)
  
}

# lme4::lmer(ise2S ~ 1 + (1|cod_mod7), data = bdfin) %>% calc_icc()

# hist(pca_dos$puntajes)
# length(unique(bdfin$ID))
# bdfin <- mutate(bdfin, id2 = paste0(cor_minedu, cor_est))
# length(unique(bdfin$id2))
# n_occur <- data.frame(table(bdfin$id2))
# n_occur[n_occur$Freq > 1,]

ggplot(bdfin, aes(ise2S)) +  geom_density(size = 2)

rio::export(bdfin, here("01-data", "03-intermedias", "02b-puntajes-ise",  "EM22_est2S_ise.rds"))
# rio::export(bdfin, here("01-data", "03-intermedias", "02b-puntajes-ise",  "EM22_est2S_ise.sav"))

# rio::export(bdfin, here("01-data", "03-intermedias", "02b-puntajes-ise", paste0(names(bd_complete_l[1]), "_ise.rds")))

# ***********************

show_in_excel <- function(.data){
  tmp <- paste0(tempfile(), ".xlsx") 
  rio::export(.data, tmp)
  browseURL(url = tmp)
} 


map(pca_uno, "indicadores") %>% 
  bind_rows(.id = "indicador") %>%
  show_in_excel()

pca_dos$cargas
pca_dos$indicadores

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


# ***************************************************************

bd19 <- rio::import("D:/1. UMC/2022/2-exploración-ise/ECE22_ise_LIMA.xlsx")
bd19 <- select(bd19, cod_mod7, ise2S) %>% mutate(a = 2019)

bd22 <- select(bdfin, cod_mod7, ise2S) %>% mutate(a = 2022)

bb <- bind_rows(bd19, bd22)

bb2 <- bb %>%
  group_by(a, cod_mod7) %>%
  summarise(mean1 = mean(ise2S, na.rm = TRUE),
            nn = n()) %>%
  arrange(cod_mod7)

ggplot(bb2, aes(x = a, y = mean1, gt)) + 
  geom_line(aes(group = as.factor(cod_mod7))) + 
  geom_point() 

ggplot(bb2, aes(mean1)) + 
  geom_density(aes(color = as.factor(a)))

padron <- rio::import("D:/1. UMC/2023/00-padron/Padron_web.dbf")
names(padron)
padron2 <- select(padron, cod_mod7 = COD_MOD, D_GESTION)

bb3 <- left_join(bb2, padron2, by = "cod_mod7")

bb3 <- mutate(bb3, gestion = ifelse(D_GESTION == "Privada", "Privada", "Publica"))

table(bb3$a, bb3$gestion)

library(lme4)
dfest <- left_join(bb, padron2, by = "cod_mod7")
dfest <- mutate(dfest, gestion = ifelse(D_GESTION == "Privada", "Privada", "Publica"))


lmer(ise2S ~ 1 + (1|cod_mod7), data = filter(dfest, a == 2019)) %>% calc_icc()
lmer(ise2S ~ 1 + (1|cod_mod7), data = filter(dfest, a == 2019, gestion == "Privada")) %>% calc_icc()
lmer(ise2S ~ 1 + (1|cod_mod7), data = filter(dfest, a == 2019, gestion == "Publica")) %>% calc_icc()

lmer(ise2S ~ 1 + (1|cod_mod7), data = filter(dfest, a == 2022)) %>% calc_icc()
lmer(ise2S ~ 1 + (1|cod_mod7), data = filter(dfest, a == 2019, gestion == "Privada")) %>% calc_icc()
lmer(ise2S ~ 1 + (1|cod_mod7), data = filter(dfest, a == 2019, gestion == "Publica")) %>% calc_icc()


