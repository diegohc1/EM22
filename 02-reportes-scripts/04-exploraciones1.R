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
vv <- names(bd)[startsWith(names(bd), "EST")]

# lmer(ise2S~1 + (1|cod_mod7), data = bd1) %>% calc_icc()

bd %>%
  select(all_of(vv), ise2S) %>%
  cor2()

ggplot(bd, aes(y = EST2SMAT_DISRP, x = ise2S)) + 
  geom_point()

hist(bd$ise2S)
hist(bd$EST2SMAT_DISRP)

lmer(EST2SGEN_PRAER ~ ise2S + (1|cod_mod7), data = bd) %>% fixef()

map(vv, ~lmer(paste(.x, "~ ise2S + (1|cod_mod7)"), data = bd) %>% fixef())


bd1 <- bd %>%
  select(cod_mod7, EST2SLEC_ACTREFL, EST2SMAT_DISRP, EST2SMAT_ACTCOG, EST2SMAT_EVFORM, ise2S)

lmer(EST2SLEC_ACTREFL~1 + (1|cod_mod7), data = bd1) %>% calc_icc()
lmer(EST2SMAT_DISRP~1 + (1|cod_mod7), data = bd1) %>% calc_icc()
lmer(EST2SMAT_ACTCOG~1 + (1|cod_mod7), data = bd1) %>% calc_icc()
lmer(EST2SMAT_EVFORM~1 + (1|cod_mod7), data = bd1) %>% calc_icc()

# ICC(1)
icc1 <- lmer(EST2SMAT_DISRP~1 + (1|cod_mod7), data = bd1) %>% calc_icc()

# ICC(2)
k <- bd1 %>%
  group_by(cod_mod7) %>%
  mutate(n = n()) %%

k <- mean(bdfinal$n)
# k*ICC(1) / 1 + (k-1)*ICC
icc2 <- k*icc1 / (1 + (k - 1)*icc1)




















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

