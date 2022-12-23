# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Exploraciones 1 
# *****************************************************************************************************************************

library(tidyverse)
library(here)
library(lme4)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")

lista = rio::import_list(Sys.glob(here("01-data", "04-para-el-analisis", "*.sav")))

bd <- lista$EM2022_2Sestudiante_EBRD2
bdlm <- filter(bd, !provincia %in% c("HUAROCHIRÍ", "CANTA"))

unique(bd$provincia)
vv <- names(bd)[startsWith(names(bd), "EST")]

# lmer(ise2S~1 + (1|cod_mod7), data = bd) %>% calc_icc()

ggplot(bd, aes(y = EST2SMAT_DISRP, x = ise2S)) + geom_point()

hist(bd$ise2S)
hist(bd$EST2SMAT_DISRP)

bd %>% select(ise2S, starts_with("EST")) %>% cor2()

map(vv, ~lmer(paste(.x, "~ ise2S + (1|cod_mod7)"), data = bd) %>% fixef())

bd %>%
  group_by(cod_mod7) %>%
  select(ise2S, starts_with("EST")) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  select(-cod_mod7) %>%
  cor2()


padron <- rio::import("D:/1. UMC/2023/00-padron/Padron_web.dbf")
names(padron)
padron2 <- select(padron, cod_mod7 = COD_MOD, D_GESTION)

bb3 <- left_join(bdlm, padron2, by = "cod_mod7")
bb3 <- mutate(bb3, gestion = ifelse(D_GESTION == "Privada", "Privada", "Publica"))

table(bb3$gestion)

bb3$ise2S <- attr(bb3$ise2S, "ISE")



bb3 %>%
  group_by(gestion) %>%
  select(ise2S, starts_with("EST")) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(-gestion, names_to = "var", values_to = "est") %>%
  ggplot(aes(x = var, y = est, color = gestion)) + 
  geom_point(size = 4)


bb3 %>%
  group_by(cod_mod7) %>%
  summarise(across(starts_with("EST"), mean, na.rm = TRUE),
            gestion = first(gestion)) %>%
  group_by(gestion) %>%
  summarise(across(starts_with("EST"), mean, na.rm = TRUE)) %>%
  pivot_longer(-gestion, names_to = "var", values_to = "est") %>%
  ggplot(aes(x = var, y = est, color = gestion)) + 
  geom_point(size = 4)


# pegado con docente ------ 

bddoc <- lista$EM2022_2SdocenteCOM_EBR
bddoc <- select(bddoc, cor_minedu, starts_with("DOC"))

bdie <- bd %>%
  group_by(cod_mod7) %>%
  summarise(isep = mean(ise2S, na.rm = TRUE),
            across(starts_with("EST"), mean, na.rm = TRUE),
            cor_minedu = first(cor_minedu))

df <- left_join(bdie, bddoc, by = "cor_minedu")

# esta bien pegado, mal pegado? para el martes! 
df %>%
  select(isep,  starts_with("DOC")) %>%
  cor2()

lm(DOC2SGEN_BURN2~isep, data = df) %>% summary()
lm(DOC2SLEC_CLCOAPR ~isep, data = df) %>% summary()
lm(DOC2SLEC_CLCOTN ~isep, data = df) %>% summary()
lm(DOC2SGEN_PERTECNEG ~isep, data = df) %>% summary()
lm(DOC2SGEN_PERTECPOS ~isep, data = df) %>% summary()

# confiabilidad de variables de nivel 2 reportadas en nivel 1 ----

bd1 <- bd %>%
  select(cod_mod7, EST2SLEC_ACTREFL, EST2SMAT_DISRP, EST2SMAT_ACTCOG, EST2SMAT_EVFORM, ise2S)

lmer(EST2SLEC_ACTREFL~1 + (1|cod_mod7), data = bd1) %>% calc_icc()
lmer(EST2SMAT_DISRP~1 + (1|cod_mod7), data = bd1) %>% calc_icc()
lmer(EST2SMAT_ACTCOG~1 + (1|cod_mod7), data = bd1) %>% calc_icc()
lmer(EST2SMAT_EVFORM~1 + (1|cod_mod7), data = bd1) %>% calc_icc()

# ICC(1)
icc1 <- lmer(EST2SMAT_EVFORM~1 + (1|cod_mod7), data = bd1) %>% calc_icc()

# ICC(2)
k <- bd1 %>%
  group_by(cod_mod7) %>%
  mutate(n = n()) %>%
  pull(n) %>%
  mean()

# k*ICC(1) / 1 + (k-1)*ICC
(icc2 <- k*icc1 / (1 + (k - 1)*icc1))





# Perfil latente 
bdmat <- select(bd, cod_mod7, EST2SMAT_DISRP, EST2SMAT_ACTCOG, EST2SMAT_EVFORM)
bdmat <- drop_na(bdmat)

install.packages("mclust")
library(mclust)

BIC <- mclustBIC(bdmat[-1])
plot(BIC)
summary(BIC)

mod1 <- Mclust(bdmat[-1], modelNames = "VEE", G = 3, x = BIC)
summary(mod1)

means <- data.frame(mod1$parameters$mean) %>%
  rownames_to_column() %>%
  rename(Interest = rowname) %>%
  pivot_longer(cols = c(X1, X2, X3), names_to = "Profile", values_to = "Mean") %>%
  mutate(Mean = round(Mean, 2),
         Mean = ifelse(Mean > 1, 1, Mean))

means %>%
  ggplot(aes(Interest, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  labs(x = NULL, y = "Standardized mean interest") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

