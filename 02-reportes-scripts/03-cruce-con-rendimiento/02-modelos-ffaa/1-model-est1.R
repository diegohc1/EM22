# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# # Modelos multinivel: estudiante y familia nivel 1 
# *****************************************************************************************************************************

library(here)
library(tidyverse)
library(MplusAutomation)
# devtools::install_github("diegohc1/factorito")


reg_mplus <- function(data, y, x1 = NULL, x2 = NULL, peso_est, idie){
  
  if(!require(glue)) stop("'glue' debe estar instalado")
  
  if (is.null(x2)) {x2 = ""} else x2 <- paste(x2, collapse = " ")
  if (is.null(x1)) {x1 = ""} else x1 <- paste(x1, collapse = " ")
  
  # definimos las variables 
  vars <- 
    glue("CLUSTER = {idie};
     WEIGHT = {peso_est};
     BWEIGHT  = peso_ie;
     WTSCALE  = CLUSTER;
     BWTSCALE = SAMPLE;
     USEVARIABLES = {y} {x1} {x2};
     WITHIN = {x1};
     BETWEEN = {x2};")
  
  # definimos los modelos
  if (is.null(x1)){ # solo nivel2
    mod <- glue("%BETWEEN% \n {y} on {x2};")
  } else if (is.null(x2)){ # solo nivel 1
    mod <- glue("%WITHIN% \n {y} on {x1};")
  } else { #todos
    mod <- glue("%WITHIN% \n {y} on {x1}; \n %BETWEEN% \n {y} on {x2};")
  }
  
  # objeto mplus 
  text_mplus <- mplusObject(
    TITLE = "Modelo XXX;", 
    VARIABLE = vars,
    ANALYSIS = "type = TWOLEVEL;" ,
    MODEL = mod,
    rdata = data,
  )
  
  # corremos modelo y lo regresamos 
  mplus_out <- MplusAutomation::mplusModeler(
    text_mplus, 
    dataout = here("02-reportes-scripts", "03-cruce-con-rendimiento", "02-modelos-ffaa", "mods", "mlm.dat"),
    modelout = here("02-reportes-scripts", "03-cruce-con-rendimiento", "02-modelos-ffaa", "mods", "mlm.inp"),
    check = TRUE, run = TRUE, hashfilename = FALSE)
  
}
mpluscoef <- function(mplus_salida) return(mplus_salida$results$parameters$unstandardized) 

# y_ij = b0 + u_00 + b01*x_ij + e_ij

load(here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))

# mate 
bd1 <- rend2sl$EM2022_2Sestudiante_EBRD2 %>%
  select(M500_EM_2S_2022_MA, Peso_mate, Peso_IE_mate, cod_mod7.x, starts_with("EST"), ise2S) %>%
  rename(peso_ie = Peso_IE_mate, cod_mod7 = cod_mod7.x) %>%
  group_by(cod_mod7) %>%
  mutate(isep = mean(ise2S, na.rm = TRUE)) %>%
  drop_na(M500_EM_2S_2022_MA, Peso_mate, peso_ie)

mise <- reg_mplus(bd1, y = "M500_EM_2S_2022_MA", x1 = "ise2S" , peso_est = "Peso_mate", idie = "cod_mod7")
misep <- reg_mplus(bd1, y = "M500_EM_2S_2022_MA", x2 = "isep" , peso_est = "Peso_mate", idie = "cod_mod7")
mise2 <- reg_mplus(bd1, y = "M500_EM_2S_2022_MA", x1 = "ise2S", x2 = "isep" , peso_est = "Peso_mate", idie = "cod_mod7")
mpluscoef(mise); mpluscoef(misep); mpluscoef(mise2)

bb <- map(list(mise, misep, mise2), ~.x[["results"]]) 
screenreg(bb %>% map(., tt) )


m1 <- reg_mplus(bd1, y = "M500_EM_2S_2022_MA", x1 = "EST2SMAT_AUTOEF" , peso_est = "Peso_mate", idie = "cod_mod7")
m2 <- reg_mplus(bd1, y = "M500_EM_2S_2022_MA", x1 = "EST2SMAT_AUTOEF" , peso_est = "Peso_mate", idie = "cod_mod7")

m2 <- reg_mplus(bd1, y = "M500_EM_2S_2022_MA", x1 = c("EST2SMAT_AUTOEF", "ise2S"), peso_est = "Peso_mate", idie = "cod_mod7")
mpluscoef(m1)
mpluscoef(m2)


# lectura 
bd1 <- rend2sl$EM2022_2Sestudiante_EBRD2 %>%
  select(M500_EM_2S_2022_CT, Peso_lectura, Peso_IE_lectura, cod_mod7.x, starts_with("EST"), ise2S) %>%
  rename(peso_ie = Peso_IE_lectura, cod_mod7 = cod_mod7.x) %>%
  group_by(cod_mod7) %>%
  mutate(isep = mean(ise2S, na.rm = TRUE)) %>%
  drop_na(M500_EM_2S_2022_CT, Peso_lectura, peso_ie)
names(bd1)

mise <- reg_mplus(bd1, y = "M500_EM_2S_2022_CT", x1 = "ise2S" , peso_est = "Peso_lectura", idie = "cod_mod7")
misep <- reg_mplus(bd1, y = "M500_EM_2S_2022_CT", x2 = "isep" , peso_est = "Peso_lectura", idie = "cod_mod7")
mise2 <- reg_mplus(bd1, y = "M500_EM_2S_2022_CT", x1 = "ise2S", x2 = "isep" , peso_est = "Peso_lectura", idie = "cod_mod7")
mpluscoef(mise); mpluscoef(misep); mpluscoef(mise2)


m1 <- reg_mplus(bd1, y = "M500_EM_2S_2022_CT", x1 = "EST2SLEC_ESTLEC" , peso_est = "Peso_lectura", idie = "cod_mod7")
m2 <- reg_mplus(bd1, y = "M500_EM_2S_2022_CT", x1 = c("EST2SLEC_ESTLEC", "ise2S"), peso_est = "Peso_lectura", idie = "cod_mod7")
mpluscoef(m1)
mpluscoef(m2)

glue::glue(c("EST2SMAT_AUTOEF", "ise2S"))

xx <- c("EST2SMAT_AUTOEF", "ise2S")

glue("aaa, {xx}")
paste("aaa", xx)
xxxx <- paste(xx, collapse = " ")

glue("aaa {xxxx}")

names(rend2sl$EM2022_2Sestudiante_EBRD2)


# renombra <- function(x) paste0(str_sub(x,1,3), str_sub(sub(".*_", "", x),1,5))



