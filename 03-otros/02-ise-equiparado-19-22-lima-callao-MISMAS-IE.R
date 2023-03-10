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

# 2022

#bases
lista = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "01b-imputadas-ise", "*.rds"))) 

# quedarnos con LIMA 
d <- Sys.glob(here("01-data", "02-con-etiquetas", "*.sav"))
temp <- rio::import(d[5])
temp <- select(temp, cod_mod7, provinciaX) %>% distinct(cod_mod7, .keep_all = TRUE)
temp %>% count(provinciaX)

lista[[1]] <- left_join(lista[[1]], temp, by = "cod_mod7")
bd22 <- filter(lista[[1]], !provinciaX %in% c("CANTA", "HUAROCHIRÍ"))
names(bd22)
bd22 <- select(bd22, cod_mod7, anexo, cor_minedu, cor_est, everything(), -dsc_seccion_imp, -provinciaX, -ID)
bd22 <- bd22 %>%
  mutate(seccion = substr(cor_minedu, 7, 8),
         seccion = str_pad(seccion, 2, pad = "0"))%>% 
  # select(cor_minedu) %>%
  mutate(a = 2022) %>%
  mutate(across(where(is.factor), as.numeric.factor))

# 2019
# ya esta solo con LIMA
bd19 <- rio::import("D:/1. UMC/2022/2-exploracion-ise/bdlima2019-ise.sav")
bd19 <- mutate(bd19, a = 2019) %>% select(-nom_dre)
bd19 <- filter(bd19, cod_mod7 %in% bd22$cod_mod7) 

# juntamos 
sapply(bd19, class)
sapply(bd22, class)
bdf <- bind_rows(bd22, bd19)

# Acomodaciones finales para el PCA
bdfb <- bdf %>%
  mutate(maxedu = reduce(select(., c(edum, edup)), pmax), #max educación padre y madre
         activo14_15 = ifelse(activo14 == 1 | activo15 == 1, 1, 0)
  ) 

bdfb %>%
  group_by(a) %>%
  summarise(across(starts_with("act"), mean))


# PCA
bd_list <- list(
  materiales_vivienda = select(bdfb, starts_with("mat")),
  servicios_basicos = select(bdfb, starts_with("serv")),
  activos = select(bdfb, starts_with("activo"), -activo1, -activo5, -activo14, -activo15),
  otros_serv = select(bdfb, starts_with("oserv"))
)

pca_uno <- map(bd_list, ~factorito::reporte_pca(.x, "poly"))
pca_unop <- map_df(pca_uno, 1)

pca_dos <- bind_cols(bdfb, pca_unop) %>%
  select(maxedu, materiales_vivienda, servicios_basicos, activos, otros_serv) %>%
  factorito::reporte_pca()

bdfin <- bind_cols(bdfb, ise2S = pca_dos$puntajes) %>% select(cod_mod7, cor_minedu, seccion, cor_est, ise2S, a)
names(bdfin)  

# agregarle los pesos 
# lista_rend = rio::import_list(Sys.glob(here("01-data", "05-bd-rendimiento", "*.sav")), setclass = "tibble") 
# rend <- lista_rend[[1]]
# rend <- select(rend, cor_minedu, cor_est, Peso_lectura)
# 
# bdfin <- left_join(bdfin, rend, by = c("cor_minedu", "cor_est"))
# bdfin <- mutate(bdfin, Peso_lectura = ifelse(a == 2019, 1, Peso_lectura))
# bdfin <- drop_na(bdfin, Peso_lectura)

ggplot(bdfin, aes(ise2S)) + geom_density()
ggplot(bdfin, aes(ise2S)) + geom_density(aes(color = factor(a)))
#ggplot(bdfin, aes(ise2S, weight = Peso_lectura/sum(Peso_lectura))) + geom_density(aes(color = factor(a)))

bdfin %>%
  group_by(a) %>%
  factorito::mean_prop_grupo("ise2S")

tab1 <- bdfin %>%
  group_by(cod_mod7, a) %>% 
  factorito::mean_prop_grupo("ise2S")

padron22 <- rio::import("D:/1. UMC/2023/Padron_web.dbf")
names(padron22)
padron22 <- select(padron22, cod_mod7 = COD_MOD, D_GESTION) %>%
  mutate(gestion2 = ifelse(D_GESTION == "Privada", "Privada", "Publica")) %>%
  select(-D_GESTION)

tab1 <- left_join(tab1, padron22, by = "cod_mod7")
arrange(tab1, -media)

ggplot(tab1, aes(x = as.factor(a), y = media)) + 
  geom_line(aes(group = as.factor(cod_mod7))) + 
  geom_point(aes(size = n)) +
  facet_wrap(~gestion2)

