
rm(list = ls())
options(survey.lonely.psu = "certainty") #opcion de survey

load(here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))

# MIAU 😺
# matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")


bd1 <- rend2sl$EM2022_2Sestudiante_EBRD2 


bd1 <- mutate(bd1, 
                 probseccionlec = 1/Peso_seccion_lectura,
                 probseccionmat = 1/Peso_seccion_mate,
                 probseccioncyt = 1/Peso_seccion_CN,
                 probielec = 1/Peso_IE_lectura,
                 probiemat = 1/Peso_IE_mate,
                 probiecyt = 1/Peso_IE_CN
) %>%
  drop_na(Peso_lectura)

bdlec <- bd1 %>%
  mutate(across(starts_with("p06"), ~fct_collapse(.x, 
                                                  "Nunca o pocas veces" = c("Nunca o casi nunca", "Pocas veces"),
                                                  "Muchas veces o casi siempre" = c("Muchas veces", "Siempre o casi siempre"))))

bd1$p10_05

mutate(bd1, across(starts_with("p10"), 
                   ~fct_collapse(.x, 
                                "En desacuerdo o totalmente en desacuerdo" = c("Totalmente en desacuerdo", "En desacuerdo"),
                                "De acuerdo o totalmente de acuerdo" = c("De acuerdo", "Totalmente de acuerdo")))) %>%
  count(p10_01)

mutate(bd1, across(starts_with("p10"), 
                   ~fct_recode(.x, 
                                 "En desacuerdo o totalmente en desacuerdo" = "Totalmente en desacuerdo", 
                               "En desacuerdo o totalmente en desacuerdo" = "En desacuerdo",
                                 "De acuerdo o totalmente de acuerdo" = "De acuerdo", 
                               "De acuerdo o totalmente de acuerdo" = "Totalmente de acuerdo"))) %>%
  count(p10_01)


ll <- setNames(
  c("Totalmente en desacuerdo", "En desacuerdo", "De acuerdo", "Totalmente de acuerdo"),
  c(rep("En desacuerdo o totalmente en desacuerdo", 2), rep("De acuerdo o totalmente de acuerdo", 2))
  )

mutate(bd1, across(starts_with("p10"), ~fct_recode(.x, !!!ll))) %>%
    count(p10_01)


mutate(bd1, across(starts_with("p10"), ~fct_recode(.x, !!!recodefac$acuerdo))) 


library(survey)
names(bd1)
bdlec_svy <- svydesign(data = bdlec, 
                       id = ~cod_mod7 + ID_seccion, 
                       strata = ~Estrato, 
                       fpc = ~probielec + probseccionlec, 
                       nest = TRUE, 
                       pps = "brewer")

nom <- names(select(bd1, starts_with(c("p0", "p1"))))
nom1 <- names(select(bd1, starts_with(c("p06"))))
t1 <- svyby(~M500_EM_2S_2022_CT, ~p06_01, bdlec_svy, svymean)

ini <- Sys.time()
bb <- lapply(nom, function(x) svyby(~M500_EM_2S_2022_CT, as.formula(paste0("~", x)), bdlec_svy, svymean))
fin <- Sys.time() 
fin - ini

ini <- Sys.time()
# library(parallel)
n_cores <- detectCores()
cl <- makeCluster(n_cores - 1)  
clusterEvalQ(cl, {library("survey"); options(survey.lonely.psu = "certainty") })
clusterExport(cl, list('nom1', 'bdlec_svy'))
#ini <- Sys.time()
  aa <- parLapply(cl, nom, function(x) svyby(~M500_EM_2S_2022_CT, as.formula(paste0("~", x)), bdlec_svy, svymean))
#fin <- Sys.time() 
#fin - ini
stopCluster(cl)

fin <- Sys.time() 
fin - ini

setNames(aa, nom) %>%
  lapply(function(x) rename(x, opcion = 1)) %>%
  bind_rows(.id = "var") %>%
  `rownames<-`( NULL )



factorito::dif_sig(t1, m = "M500_EM_2S_2022_CT", se = "se")




