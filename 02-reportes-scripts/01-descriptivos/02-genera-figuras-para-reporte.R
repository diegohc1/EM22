# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# Generar figuras con los porcentajes de respuesta
# *****************************************************************************************************************************

# (1) figuras para el reporte PDF (reporte inicial)

rm(list = ls())

library(tidyverse)
library(here)

acomoda_poslabel <- function(data, freq, xn){
  
  label_centro <- function(x){cumsum(x) - x*0.5} 
  data %>% mutate(
    pos_label = label_centro({{ freq }}),
    pos_label = ifelse({{ xn }} == 1 & {{ freq }} < 3, -2, pos_label),
    col_label = ifelse({{ xn }} == 1 & {{ freq }} >= 3 & max({{ xn }}) > 2, "white", "black"))
}


load(file = here("02-reportes-scripts", "01-descriptivos", "01-descriptivos-ffaa.Rdata"))

tabla <- tab_final2
tabla1 <- filter(tabla, estrato == "General")


#para ordenar factores en el grafico
# se desordena todo cuando hay opciones que nadie marcó o por alguna cuestion inexplicable 
# nos aseguramos colocando nuevamente los labels 
levels_factor <- tabla1 %>%
  select(Concatena1, cod_gen, OpcionL) %>% 
  distinct(Concatena1, cod_gen, .keep_all = T) %>%
  mutate(labelf = strsplit(as.character(OpcionL), ";")) %>%
  unnest(labelf) %>% 
  select(Concatena1, cod_gen, labelf) 


# (1) generamos graficos -----

nom <- unique(tabla1$Concatena1)

# (1.1) graficos de barras, un solo item ----

for(i in 1:length(nom)){ #i=1
  
  bd <- filter(tabla1, Concatena1 == nom[i], TipoV == "Categorico1", estrato == "General")
  vars1 <- unique(bd$cod_preg)
  
  for(j in 1:length(vars1)){ #j=1
    
    levf <- filter(levels_factor, Concatena1 == nom[i], cod_gen == vars1[j])$labelf
    
    tab <- bd %>%
      filter(cod_preg == vars1[j]) %>%
      mutate(opcion = factor(opcion, levels = levf, labels = levf))
    
    n_items <- length(unique(tab$opcion)) #para ajustar size 
    
    g <- 
      ggplot(tab, aes(x = opcion, y = prop)) + 
      geom_col(position = "dodge", fill = "#8dd3c7") + 
      theme_minimal() + labs(x = "", y = "")
    
    if(length(g$data$prop) < 9){ #si hay menos de 9 opciones, horizontal
      
      g1 <- g +  
        geom_text(aes(
          label = format(prop, decimal.mark = ",")), 
          vjust = -0.5, size = 2.8, color = "#5A5D63") +
        theme(axis.text.y = element_blank(), 
              axis.text.x = element_text(size = 8.1, color = "#5A5D63"),
              panel.grid = element_blank(),
              plot.margin = unit(c(.5, .3, .3, .3), "cm")) +
        scale_x_discrete(labels = str_wrap(unique(tab$opcion), width = 25)) + 
        coord_cartesian(ylim = c(0, max(tab$prop) + 3))
      
    }else{ #si hay mas de 9 opciones, vertical
      
      g1 <- g + 
        coord_flip() +  
        geom_text(aes(
          label = format(prop, decimal.mark = ",")), 
          vjust = -0.5, hjust = -.2, size = 2.8, color = "#5A5D63") +
        theme(axis.text.x = element_blank(), 
              axis.text.y = element_text(size = 8.1, color = "#5A5D63", hjust = 0),
              panel.grid = element_blank(),
              plot.margin = unit(c(0, .3, .3, .3), "cm")) +
        scale_x_discrete(labels = str_wrap(unique(tab$opcion), width = 55)) 
    }
    
    ggsave(
      here("02-reportes-scripts", "01-descriptivos", "figuras-para-reportes", "00-nacional", paste0("g_", nom[i], "_", vars1[j], ".pdf")),
      g1,
      w = ifelse(n_items > 8, 10.687500, n_items*1.675), 
      h = ifelse(n_items > 8, 5.885417, 3.408333), 
      dpi = 500) 
    
  }
  
}


# (1.2) graficos de barras, varios item ----

for(i in 1:length(nom)){ #i=4
  
  bd <- filter(tabla1, Concatena1 == nom[i], TipoV == "Categorico2", estrato == "General")
  vars1 <- unique(bd$cod_gen)
  
  for(j in 1:length(vars1)){ #j=4
    
    levf <- filter(levels_factor, Concatena1 == nom[i], cod_gen == vars1[j])$labelf
    
    tab2 <- bd %>%
      select(cod_gen:n, opcion, cod_preg, Enunciado, freq = prop) %>%
      filter(cod_gen == vars1[j]) %>%
      mutate(
        opcion = factor(opcion, levels = levf, labels = levf),
        xn = as.numeric(opcion),
        enunciado_wrap = str_wrap(paste0(cod_preg, " ", Enunciado), 60)) %>%
      filter(freq != 0) %>%
      group_by(cod_preg) %>%
      acomoda_poslabel(freq, xn) #%>% #posicion de los labels (deben estar agrupados)
    #ajuste_poslabel2(pos_label, freq, xn) #si es que las posiciones de los labels estan muy cerca
    
    #definimos algunos parametros
    ley <- ifelse(sum(nchar(levels(tab2$opcion))) > 120, 0.175, 0.30) #para ajustar pos leyenda
    n_items <- length(unique(tab2$cod_preg)) #para ajustar tamaño figura
    letras <- sum(nchar(unique(tab2$enunciado_wrap)))/n_items #para ajustar tamaño figura
    
    theme_1 <- theme(panel.grid = element_blank(),
                     axis.text.y = element_text(size = 8.1, color = "#5A5D63", hjust = 0),
                     axis.ticks = element_blank(), axis.text.x=element_blank(),
                     plot.margin = unit(c(0.2, 0, 0.9, 0), "cm"))
    
    g <- ggplot(data = tab2) + 
      geom_bar(
        aes(x = fct_rev(cod_preg), y = freq, fill = fct_rev(opcion)), 
        stat = "identity", position = "stack") + 
      geom_text(
        aes(label = format(freq, decimal.mark = ","), y = pos_label, x = cod_preg), 
        vjust = 0.5, size = 2.6, color = tab2$col_label, check_overlap = TRUE) + 
      theme_minimal() + coord_flip() + labs(x = "", y = "") + 
      scale_fill_brewer("BuPu", guide = guide_legend(reverse = TRUE, direction = "horizontal", nrow = 1)) +
      theme_1 + 
      theme(legend.position = c(ley, -0.08), legend.title = element_blank(),
            legend.key.size = unit(.9,"line"), legend.text = element_text(size = 8, color = "#5A5D63")) +
      scale_x_discrete(labels = rev(unique(tab2$enunciado_wrap)))
    
    case_when( #grafico general
      n_items < 4  ~ n_items*0.8041667, 
      letras > 120  ~ n_items*0.7041667, 
      letras > 100 & n_items < 5 ~ n_items*0.5741667,
      n_items == 12 ~ n_items*0.3941667, 
      n_items > 13 ~ n_items*0.2941667,
      TRUE ~ n_items*0.5041667) -> h1
    
    ggsave(
      here("02-reportes-scripts", "01-descriptivos", "figuras-para-reportes", "00-nacional", paste0("g_", nom[i], "_", vars1[j], ".pdf")),
      g, 
      w = 9.052083 , 
      h = h1, 
      dpi = 500) 
  }
}


# # (2) generamos reporte para cada cuestionario -----
# 
# # reporte por cuestionario -----
# 
# load(file = here("4-descriptivos", "01-descriptivos-ffaa.Rdata"))
# cuest <- unique(tab_final2$Concatena2)
# 
# for (i in 1:length(cuest)) { #i=1 
#   rmarkdown::render(
#     input = here("4-descriptivos", "03-estructura-reporte.Rmd"),
#     params = list(cuest = cuest[i]),
#     output_file = here("4-descriptivos", "03-reportes", paste0(cuest[i], ".pdf"))
#   )
# }
