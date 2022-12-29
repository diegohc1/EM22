
library(tidyverse)
library(here)
library(lme4)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")

lista = rio::import_list(Sys.glob(here("01-data", "04-para-el-analisis", "*.sav")))

rendlima <- rio::import(here("01-data", "EM_2S_2022_todo_P_eqp_con_corte.sav"))
names(rendlima)
bdest <- lista$EM2022_2Sestudiante_EBRD2 

bdest <- mutate(bdest, ID_estudiante = paste0(cor_minedu, cor_est))
bdest <- select(bdest, ID_estudiante, ise2S, starts_with("est"))
bdest <- distinct(bdest, ID_estudiante, .keep_all = TRUE)


length(unique(bdest$ID_estudiante)) - nrow(bdest)
length(unique(rendlima$ID_estudiante)) 

bd <- left_join(rendlima, bdest, by = "ID_estudiante")
names(bd)

vv <- names(bd)[startsWith(names(bd), "EST")]
vv <- vv[c(1, 2, 3, 8)]

map(vv, ~lmer(paste("medida500_L~", .x, " + (1|cod_mod7)"), data = bd) %>% fixef())
m1 <- map(vv, ~lmer(paste("medida500_L~", .x, " + (1|cod_mod7)"), data = bd))
texreg::screenreg(m1)
m2 <- map(vv, ~lmer(paste("medida500_L~", .x, "+ise2S + (1|cod_mod7)"), data = bd))
texreg::screenreg(m2)

map(vv, ~lmer(paste("medida500_L~", .x, " + (1|cod_mod7)"), data = bd) %>% fixef())
lmer(medida500_L ~ ise2S + (1|cod_mod7), data = bd)

bd <- bd %>%
  group_by(cod_mod7) %>%
  mutate(EST2SLEC_ACTREFL_p = mean(EST2SLEC_ACTREFL, na.rm = TRUE),
         EST2SMAT_DISRP_p = mean(EST2SMAT_DISRP, na.rm = TRUE),
         EST2SMAT_ACTCOG_p = mean(EST2SMAT_ACTCOG, na.rm = TRUE),
         EST2SMAT_EVFORM_p = mean(EST2SMAT_EVFORM, na.rm = TRUE),
         isep = mean(ise2S, na.rm = TRUE))

vvp <- names(bd)[endsWith(names(bd), "p")]

map(vvp, ~lmer(paste("medida500_L~", .x, "+ (1|cod_mod7)"), data = bd) %>% fixef())
m3 <- map(vvp, ~lmer(paste("medida500_L~", .x, " + (1|cod_mod7)"), data = bd))
texreg::screenreg(m3)

map(vvp[-5], ~lmer(paste("medida500_L~", .x, "+ isep + (1|cod_mod7)"), data = bd) %>% fixef())

m4 <- map(vvp, ~lmer(paste("medida500_L~", .x, " + isep + (1|cod_mod7)"), data = bd))
texreg::screenreg(m4)
