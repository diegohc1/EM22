rm(list = ls())

library(tidyverse)
library(here)

est19 <- rio::import("D:/1. UMC/2019/bases/ECE2019_EBR_2S_F1Estudiante.sav")
ece19 <- rio::import("D:/1. UMC/2019/bases/ECE 2S 2019 estudiantes.sav")

names(est19)
unique(est19$provincia)

# sentido de pertenencia ---- 

# 2019: 
# 24_01 Con frecuencia me aburro en el colegio
# 24_02: Mi colegio es un lugar donde me siento solo
# 24_03: Prefiero faltar al colegio
# 24_04: En el colegio me siento fuera de lugar
# 24_05: Mi colegio es un lugar donde me siento como un extraño o dejado de lado

# 2022
# 15_01: Preferiría faltar al colegio.
# 15_02: Mi colegio es un lugar donde me siento solo(a)
# 15_03: Preferiría estudiar en otro colegio.
# 15_04: En el colegio, me siento fuera de lugar
# 15_05: Mi colegio es un lugar donde me siento como un extraño

# iguales: 
# p24_04 y p15_04
# p24_03 y p15_01
# p24_02 y p15_02
# p24_05 y p15_05 (muy parecido, usemos sin miedo)

# acomodamos 2019 

est19b <- est19 %>%
  filter(provincia %in% c("LIMA", "CALLAO")) %>%
  select(cod_mod7, cor_minedu, cor_est, p24_02, p24_03, p24_04, p24_05) %>%
  #select(everything(), p24_02, p24_03, p24_04, p24_05) %>%
  mutate(cod_mod7 = str_pad(cod_mod7, 7, pad = "0"),
         cor_est = str_pad(cor_est, 2, pad = "0"),
         a = "2019") 
  

# acomodamos 2022 
       
est22 <- rio::import(here("01-data", "03-intermedias", "01a-imputadas", "EM2022_2Sestudiante_EBRD1.rds"))

est22b <- est22 %>%
  #filter(provincia %in% c("LIMA", "CALLAO")) %>%
  select(cod_mod7, cor_minedu, cor_est, p15_02, p15_01, p15_04, p15_05) %>%
  rename(p24_02 = p15_02, p24_03 = p15_01, p24_04 = p15_04, p24_05 = p15_05) %>%
  # select(everything(), p15_02, p15_01, p15_04, p15_05) %>%
  mutate(a = "2022")

est22b <- distinct(est22b, cor_minedu, cor_est, .keep_all = TRUE)

# juntamos 
bd1 <- bind_rows(est19b, est22b)
head(est19b)
head(est22b)
sapply(est19b, class)
sapply(est22b, class)

# cfa 
library(lavaan)
bd2 <- drop_na(bd1)

m1 <- "senper_19_22 =~ p24_02 + p24_03 + p24_04 + p24_05"
mod1 <- cfa(m1, data = bd2, estimator = "WLSMV")

fitMeasures(mod1, c("cfi","tli","rmsea","srmr"))

parameterEstimates(mod1, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(Ítem = rhs, Beta = std.all) 

puntajes1 <- as.data.frame(lavaan::lavPredict(mod1))
hist(puntajes1[[1]])

bd_sen_per <- bind_cols(bd2, senper_19_22 = puntajes1$senper_19_22)
bd_sen_per <- select(bd_sen_per, cor_minedu, cor_est, a, senper_19_22)

# indice socioeconomico  -----
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
  mutate(a = "2022") %>%
  mutate(across(where(is.factor), as.numeric.factor))

bd22 <- distinct(bd22, cor_minedu, cor_est, .keep_all = TRUE)


head(bd22)

# 2019
# ya esta solo con LIMA
bd19 <- rio::import("D:/1. UMC/2022/2-exploracion-ise/bdlima2019-ise.sav")
bd19 <- mutate(bd19, a = "2019") %>% select(-nom_dre)

ece19b <- select(ece19, cod_mod7, anexo, seccion = ID_Seccion, cor_est, cor_minedu)
head(ece19b)

bd19b <- left_join(bd19, ece19b, by = c("cod_mod7", "anexo", "seccion", "cor_est"))
bd19c <- select(bd19b, cod_mod7, anexo, cor_minedu, cor_est, a, edum:oserv5)

# juntamos 
sapply(bd19c, class)
sapply(bd22, class)
bdf <- bind_rows(bd22, bd19c)

bdf %>%
  group_by(a) %>%
  summarise(across(starts_with("act"), ~round(mean(.x), 2)))

# Acomodaciones finales para el PCA
bdfb <- bdf %>%
  mutate(maxedu = reduce(select(., c(edum, edup)), pmax), #max educación padre y madre
         activo14_15 = ifelse(activo14 == 1 | activo15 == 1, 1, 0)
  ) 

# PCA
bd_list <- list(
  materiales_vivienda = select(bdfb, starts_with("mat")),
  servicios_basicos = select(bdfb, starts_with("serv")),
  #activos = select(bdfb, starts_with("activo"), -activo14, -activo15),
  activos = select(bdfb, starts_with("activo"), -activo1, -activo5, -activo14, -activo15),
  otros_serv = select(bdfb, starts_with("oserv"))
)

pca_uno <- map(bd_list, ~factorito::reporte_pca(.x, "poly"))
pca_unop <- map_df(pca_uno, 1)

pca_dos <- bind_cols(bdfb, pca_unop) %>%
  select(maxedu, materiales_vivienda, servicios_basicos, activos, otros_serv) %>%
  factorito::reporte_pca()

bdfin <- bind_cols(bdfb, ise2S = pca_dos$puntajes) %>% select(cod_mod7, cor_minedu, seccion, cor_est, ise2S, a)
bdfin <- select(bdfin, cod_mod7, cor_minedu, cor_est, a, ise2S_19_22 = ise2S)


# juntamos 
bb <- left_join(bdfin, bd_sen_per, by = c("a", "cor_minedu", "cor_est"))
bb <- rename(bb, año = a)

rio::export(bb, here("01-data", "03-intermedias", "02b-puntajes-ise",  "FFAA-indices2019_2022.sav"))













