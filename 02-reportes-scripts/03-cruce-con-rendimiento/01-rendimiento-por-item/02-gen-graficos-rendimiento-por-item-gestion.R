# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Genera figuras rendimiento promedio por item - gestion  
# *****************************************************************************************************************************
# 

rm(list = ls())
library(tidyverse)
library(here)

str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

theme1 <- theme( 
                legend.title = element_blank(),
                #axis.text.y = element_blank(), 
                legend.position = c(0.50, -0.15),
                axis.ticks = element_blank(),
                axis.text.x = element_text(size = 8.1, color = "black"),
                plot.margin = unit(c(.3, .3, 1, .3), "cm"),
                panel.grid = element_blank())

load(here("02-reportes-scripts", "03-cruce-con-rendimiento", "01-rendimiento-por-item", "01-rend-por-item-gestion.Rdata"))
# proms <- rio::import(here("3-reportes", "3-cruces-rendimiento", "00-rendimiento-promedio.xlsx"))

# cambio para docente
# a miras de que se pueda visualizar la respuesta correcta 
demate <- c("p08", "p10", "p11", "p12", "p13", "p14", "p15", "p16", "p17")

tabla_estrato3 <- tabla_estrato3 %>%
  mutate(cod_gen = ifelse(Concatena1 == "EM2022_2SdocenteCOM_EBR" & cod_gen == "p14", cod_preg, cod_gen),
         cod_gen = ifelse(Concatena1 == "EM2022_2SdocenteMAT_EBR" & cod_gen %in% demate, cod_preg, cod_gen))

#para ordenar factores en el grafico
# se desordena todo cuando hay opciones que nadie marcó o por alguna cuestion inexplicable 
# nos aseguramos colocando nuevamente los labels 
levels_factor <- tabla_estrato3 %>%
  select(Concatena1, cod_gen, OpcionL) %>% 
  distinct(Concatena1, cod_gen, .keep_all = T) %>%
  mutate(labelf = strsplit(as.character(OpcionL), ";")) %>%
  unnest(labelf) %>% 
  select(Concatena1, cod_gen, labelf) 


tabla_estrato3 <- tabla_estrato3 %>%
  #filter(gestion == "Estatal") %>%
  mutate(media = round(media, 1))


cuest <- unique(tabla_estrato3$Concatena1)
rends <- unique(tabla_estrato3$rend)
carpeta <- c("lec2s", "mat2s", "cyt2s")

# crear carpeta 
# [deberia usar esto mas seguido creo]
# rr <- here("01-data", "ZZ-rendimiento-por-item-gestion")
# dir.create2 <- function(path) ifelse(!dir.exists(path), dir.create(path), "La carpeta ya existe!")
# dir.create2(rr)
# 
# # crear subcarpetas 
# sapply(paste0(rr, "/", carpeta), dir.create2)


for(r in 1:length(rends)){ #r=1
  
  tabla_r <- filter(tabla_estrato3, rend == rends[r])
  cuest <- unique(tabla_r$Concatena1)
  
  for(i in 1:length(cuest)){ #i=1
    
    tabla_ri <- filter(tabla_r, Concatena1 == cuest[i])
    pregs <- unique(tabla_ri$cod_preg)
    mini <- min(tabla_ri$media)
    maxi <- max(tabla_ri$media)
    
    #param <- filter(proms, Concatena1 == rends[r])
    
    for(j in 1:length(pregs)){ #j=5
      
      tabla_rij <- filter(tabla_ri, cod_preg == pregs[j])
      codo <- tabla_rij$cod_gen #para filtrar en levels_factor
      levf <- filter(levels_factor, Concatena1 == cuest[i], cod_gen == codo[1])$labelf
      
      tabla_rij <- mutate(tabla_rij, opcion = factor(opcion, levels = levf, labels = levf))
      tabla_rij <- mutate(tabla_rij, opcion = str_wrap_factor(opcion, 25))
      
      tabla_rij <- mutate(tabla_rij, 
                          preg = ifelse(TipoV == "Categorico2", paste0(Pregunta,  "\n", Enunciado), Pregunta)) %>%
        mutate(preg = str_wrap(preg, 100)) 
      
      tit <- unique(tabla_rij$preg)
      
      gg1 <- tabla_rij %>%
        ggplot(aes(x = opcion, y = media, color = gestion)) + 
        #geom_hline(yintercept = param$mean1, linetype = 2, color = "gray") +
        #geom_hline(yintercept = param$low, linetype = 3, color = "gray") +
        #geom_hline(yintercept = param$hig, linetype = 3, color = "gray") +
        geom_point(size = 3) + 
        geom_text(aes(label = format(round(media), decimal.mark = ",")), hjust = -0.3, size = 4, color = "black") +
        geom_text(aes(label = paste0("(",format(prop, decimal.mark = ","),")")), hjust = 1.2, size = 2.5, color = "black") +
        theme_bw() + labs(x = "", y = "", title = tit) + 
        theme(title = element_text(size = 8)) +
        theme1 + 
        coord_cartesian(ylim = c(mini + 10, maxi + 10)) + 
        guides(col = guide_legend(direction = "horizontal", nrow = 1))
        
      
      ff <- paste0(cuest[i], "_", pregs[j], "_", rends[r])
      # ruta <- here("02-reportes-scripts", "03-cruce-con-rendimiento", 
      #              "1-rendimiento-por-item", "02-figuras", carpeta[r], paste0(ff, ".png"))
      #ruta <- here("01-data", "ZZ-rendimiento-por-item", carpeta[r], paste0(ff, ".png"))
      ruta <- here("01-data", "ZZ-rendimiento-por-item-gestion", carpeta[r], paste0(ff, ".png"))
      
      ggsave(ruta, gg1, w =  8.572917, h = 4.906250)    
      
    }
  }
}

warnings()

