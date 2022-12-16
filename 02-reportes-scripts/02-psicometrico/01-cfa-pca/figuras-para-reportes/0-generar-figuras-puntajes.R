rm(list = ls())

# *****************************************************************************************************************************
# Generar figuras de distribución de puntajes  
# *****************************************************************************************************************************

library(tidyverse)
library(here)
library(rio)


# puntajes de escalas
lista_p = rio::import_list(Sys.glob(here("01-data", "03-intermedias", "02-puntajes-factoriales", "*.rds")), setclass = "tibble")


g_histo <- function(data, var){
  
  maxi <- max(data[[quo_name(enquo(var))]], na.rm = TRUE) + 0.3
  mini <- min(data[[quo_name(enquo(var))]], na.rm = TRUE) - 0.3
  
  ggplot(data, aes({{var}})) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "gray", bins = 50) +
    geom_density(size = 0.9) + theme_minimal() +
    xlim(mini, maxi) 
}


lista_p2 <- lista_p %>% map(~select(.x, starts_with(c("DIR", "EST", "FAM", "DOC")))) 

for(i in 1:length(lista_p2)){# i=6
  for(j in 1:ncol(lista_p2[[i]])){#j=1
    
    fac_ji <- names(lista_p2[[i]][j])
    gg <- g_histo(lista_p2[[i]], .data[[fac_ji]]) # + labs(title = "Distribución de los puntajes")
    ggsave(here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "figuras-para-reportes", paste0(fac_ji, ".pdf")), 
           gg, w = 7.489583, h = 5.541667)

  }
}
  



