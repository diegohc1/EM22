rm(list = ls())
library(here)
library(tidyverse)
library(survey)
options(survey.lonely.psu = "certainty") #opcion de survey


# rendimiento 2022

lista_rend = rio::import_list(Sys.glob(here("01-data", "05-bd-rendimiento", "*.sav")), setclass = "tibble") 
rend <- lista_rend[[1]]
names(rend)

rend22 <- select(rend, cod_mod7, ID_seccion, cor_minedu, cor_est, Estrato, starts_with("M500"), starts_with("Peso"))
rend22 <- rename(rend22, M500_L =  M500_EM_2S_2022_CT, M500_M = M500_EM_2S_2022_MA, M500_CN = M500_EM_2S_2022_CN)
rend22 <- mutate(rend22, año = "2022")

rend22 <- mutate(rend22, 
       probseccionlec = 1/Peso_seccion_lectura,
       probseccionmat = 1/Peso_seccion_mate,
       probseccioncyt = 1/Peso_seccion_CN,
       probielec = 1/Peso_IE_lectura,
       probiemat = 1/Peso_IE_mate,
       probiecyt = 1/Peso_IE_CN
       )

# ise 
ise <- rio::import(here("01-data", "03-intermedias", "02b-puntajes-ise",  "FFAA-indices2019_2022.sav"))
bd <- left_join(rend22, ise, by = c("año", "cor_minedu", "cor_est"))

bd2 <- bd %>% mutate(qise = ntile(ise2S_19_22, 4)) 

# lectura 
bd_lec <- drop_na(bd2, Peso_lectura)
  
# diseño muestral 
bdlec_svy <- svydesign(data = bd_lec, 
                      id = ~cod_mod7.x + ID_seccion, 
                      strata = ~Estrato, 
                      fpc = ~probielec + probseccionlec, 
                      nest = TRUE, 
                      pps = "brewer")

tablec <- svyby(~M500_L, ~qise, bdlec_svy, svymean)
tablec22 <- mutate(tablec, mean_up = M500_L+1.39*se, mean_low = M500_L-1.39*se)
# group_by(bd_lec, qise) %>% factorito::mean_prop_grupo("M500_L", "Peso_lectura") %>% View()

# mate 
bd_mat <- drop_na(bd2, Peso_mate)

# diseño muestral 
bdmat_svy <- svydesign(data = bd_mat, 
                       id = ~cod_mod7.x + ID_seccion, 
                       strata = ~Estrato, 
                       fpc = ~probiemat + probseccionmat, 
                       nest = TRUE, 
                       pps = "brewer")

tabmat <- svyby(~M500_M, ~qise, bdmat_svy, svymean)

mutate(tabmat, se_up = 1.96*se, se_low = -1.96*se)
tab <- mutate(tabmat, mean_up = M500_M+1.39*se, mean_low = M500_M-1.39*se)

ggplot() +
  geom_point(data = tablec22, aes(y = M500_L, x = qise)) + 
  geom_errorbar(data = tablec22, aes(x = qise, y =  M500_L, ymin = mean_low, ymax = mean_up), width = 0.2) +
  geom_point(data = tab19, aes(y = media, x = qise), color = "blue") + 
  theme_bw()


lapply(1:4, function(x) confint(svymean(~M500_M , subset(bdmat_svy, qise == x))))
#confint(svymean(~M500_M , subset(bdmat_svy, qise == 4))) # intervalo de confianza


# rendimiento 2019
ece19 <- rio::import("D:/1. UMC/2019/bases/ECE 2S 2019 estudiantes.sav")
ece19b <- select(ece19, cor_minedu, cor_est, starts_with("M500"))
ece19b <- mutate(ece19b, año = "2019")
bd19 <- left_join(ece19b, ise, by = c("año", "cor_minedu", "cor_est"))
bd19 <- bd19 %>% mutate(qise = ntile(ise2S_19_22, 4))
bd19b <- drop_na(bd19, cod_mod7)

tab19 <- bd19b %>%
  group_by(qise) %>%
  factorito::mean_prop_grupo("M500_L")


filter(bd, año == "2022") %>%
  mutate(qise = ntile(ise2S_19_22, 4)) %>%
  group_by(qise) %>%
  drop_na(Peso_lectura) %>%
  factorito::mean_prop_grupo("M500_L", "Peso_lectura") %>%
  
filter(bd, año == "2019") %>%
  mutate(qise = ntile(ise2S_19_22, 4)) %>%
  group_by(qise) %>%
  #drop_na(Peso_lectura) %>%
  factorito::mean_prop_grupo("M500_L")


tab2 <- bind_cols(
  
  filter(bd, año == "2022") %>%
    mutate(qise = ntile(ise2S_19_22, 4)) %>%
    group_by(qise) %>%
    drop_na(Peso_lectura) %>%
    factorito::mean_prop_grupo("M500_L", "Peso_lectura") %>%
    select(qise, media22 = media),
  
  filter(bd, año == "2019") %>%
    mutate(qise = ntile(ise2S_19_22, 4)) %>%
    group_by(qise) %>%
    factorito::mean_prop_grupo("M500_L") %>%
    select(media19 = media)
)

tab2 %>%
  mutate(dif = media22 - media19) %>%
  show_in_excel()



lista_rend = rio::import_list(Sys.glob(here("01-data", "05-bd-rendimiento", "*.sav")), setclass = "tibble") 
rend <- lista_rend[[1]]

# install.packages("survey")
library(survey)

bb <- drop_na(rend, Peso_lectura)

bd4p_svy <- svydesign(data = bb, 
                      id = ~cod_mod7 + ID_seccion, 
                      strata = ~Estrato_DRE, 
                      fpc = ~pikIE + probsec, 
                      nest = TRUE, 
                      pps = "brewer")










