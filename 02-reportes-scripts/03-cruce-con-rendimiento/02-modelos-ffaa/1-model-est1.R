# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# # Modelos multinivel: estudiante y familia nivel 1 
# *****************************************************************************************************************************

library(here)
library(tidyverse)
library(MplusAutomation)

reg_mplus <- function(data, y, x1 = NULL, x2 = NULL, peso_est, idie){
  
  if(!require(glue)) stop("'glue' debe estar instalado")
  
  if (is.null(x2)) {x2 = ""}
  if (is.null(x1)) {x1 = ""}
  
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
    dataout = here("02-reportes-scripts", "03-cruce-con-rendimiento", "2-modelos-ffaa", "mods", "mlm.dat"),
    modelout = here("02-reportes-scripts", "03-cruce-con-rendimiento", "2-modelos-ffaa", "mods", "mlm.inp"),
    check = TRUE, run = TRUE, hashfilename = FALSE)
  
}
mpluscoef <- function(mplus_salida) return(mplus_salida$results$parameters$unstandardized) 

# y_ij = b0 + u_00 + b01*x_ij + e_ij

load(here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))

# mate 

bd1 <- rend2sl$EM2022_2Sestudiante_EBRD2 %>%
  select(M500_EM_2S_2022_MA, Peso_mate, Peso_IE_mate, cod_mod7.x, starts_with("EST")) %>%
  rename(peso_ie = Peso_IE_mate, cod_mod7 = cod_mod7.x) %>%
  drop_na(M500_EM_2S_2022_MA, Peso_mate, peso_ie)

m1 <- reg_mplus(bd1, y = "M500_EM_2S_2022_MA", x1 = "EST2SMAT_AUTOEF", peso_est = "Peso_mate", idie = "cod_mod7")
m1$results

mpluscoef(m1)
names(bd1)


names(rend2sl$EM2022_2Sestudiante_EBRD2)


# renombra <- function(x) paste0(str_sub(x,1,3), str_sub(sub(".*_", "", x),1,5))



