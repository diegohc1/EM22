
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

# install.packages("survey")
library(survey)

bb <- drop_na(rend, Peso_lectura)

bd4p_svy <- svydesign(data = bb, 
                      id = ~cod_mod7 + ID_seccion, 
                      strata = ~Estrato_DRE, 
                      fpc = ~pikIE + probsec, 
                      nest = TRUE, 
                      pps = "brewer")


