
library(here)
library(tidyverse)

# rendimiento 

lista_rend = rio::import_list(Sys.glob(here("01-data", "05-bd-rendimiento", "*.sav")), setclass = "tibble") 
rend <- lista_rend[[1]]
names(rend)

rend <- select(rend, cor_minedu, cor_est, starts_with("M500"), starts_with("Peso"))
names(rend)
rend <- rename(rend, M500_L =  M500_EM_2S_2022_CT, M500_M = M500_EM_2S_2022_MA, M500_CN = M500_EM_2S_2022_CN)
rend <- mutate(rend, año = "2022")

ece19 <- rio::import("D:/1. UMC/2019/bases/ECE 2S 2019 estudiantes.sav")
ece19b <- select(ece19, cor_minedu, cor_est, starts_with("M500"))
ece19b <- mutate(ece19b, año = "2019")

bdrend <- bind_rows(rend, ece19b)

# ise 
ise <- rio::import(here("01-data", "03-intermedias", "02b-puntajes-ise",  "FFAA-indices2019_2022.sav"))
bd <- left_join(bdrend, ise, by = c("año", "cor_minedu", "cor_est"))


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

ise <- rio::import(here("01-data", "03-intermedias", "02b-puntajes-ise",  "FFAA-indices2019_2022.sav"))
ise <- filter(ise, año == "2022")
names(ise)

rend <- left_join(rend, ise, by = c("cor_minedu", "cor_est"))
rend <- rend %>% mutate(iseq = ntile(ise2S_19_22, 4))

# install.packages("survey")
library(survey)
options(survey.lonely.psu = "certainty") #opcion de survey


bd_lec <- drop_na(rend, Peso_lectura)
bd_lec <- mutate(bd_lec, pikie_lec = 1/Peso_IE_lectura,
                 prob_seccion_lec = 1/Peso_seccion_lectura)

bd_lec_svy <- svydesign(data = bd_lec, 
                      id = ~cod_mod7.x + ID_seccion, 
                      strata = ~Estrato, 
                      fpc = ~pikie_lec + prob_seccion_lec, 
                      nest = TRUE, 
                      pps = "brewer")

m1 <- svyby(~M500_EM_2S_2022_CT, ~iseq, bd_lec_svy, svymean)
m1

# install.packages("srvyr")
library(srvyr)

bd4p_mat_srvyr <- bd_mat %>%
  as_survey_design(ids = c("cod_mod7", "ID_Seccion"), 
                   strata = "Estrato_DRE", 
                   fpc = c("pikIE", "probseccionM"), 
                   nest = TRUE,
                   pps = "brewer")


# error estandar de una poblacion  ?

tab19 <- filter(bd, año == "2019", !is.na(ise2S_19_22)) %>%
  mutate(qise = ntile(ise2S_19_22, 4)) %>%
  group_by(qise) %>%
  summarise(
    mean1 = mean(M500_M, na.rm = TRUE),
    sd1 = sd(M500_M, na.rm = TRUE),
    n1 = n()
  )

tab19 <- tab19 %>%
  mutate(se = sd1/sqrt(n1), año = "2019") %>%
  select(qise, año, mean1, se)



m1 <- svyby(~M500_EM_2S_2022_CT, ~iseq, bd_lec_svy, svymean)
tab22 <- as.data.frame(m1) %>%
  rename(qise = iseq, mean1 = M500_EM_2S_2022_CT) %>%
  mutate(año = "2022")

tabfinal <- bind_rows(tab22, tab19)

tabfinal %>%
  split(list(.$qise)) %>%
  lapply(., function(x) factorito::dif_sig(x, m = "mean1", se = "se", nom = "año", long = TRUE)) %>%
  bind_rows(.id = "tipo") %>%
  separate(col = tipo, into = c("Tipo", "Tipo_medida"))


