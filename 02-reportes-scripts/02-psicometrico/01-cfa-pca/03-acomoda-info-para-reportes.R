# *****************************************************************************************************************************
# Evaluación Muestral (EM) 22 
# *****************************************************************************************************************************
# # Acomoda la info para el reporte excel y el reporte PDF
# *****************************************************************************************************************************
# (1) acomodar en excel
# (2) acomodar para el reporte en PDF
# (3) genera los reportes! 

rm(list = ls())
library(rio)
library(tidyverse)
library(here)
devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")

# insumos
load(here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "02-info-escalas-para-reporte.Rdata")) #guardamos

# MIAU 😺
matriz <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l8fGxnB3vL7sF3fLE2Dheqopb1Ykg0DTrboFZWEcRKM/edit#gid=0")

matriz_cfa <- matriz %>%
  filter(Analisis2 %in% c("CFA", "PCA")) %>%
  drop_na(Cod_indice) %>%
  select(Concatena1, Constructo = Constructo_indicador, sub_escala, starts_with("Cod"), Enunciado,
         Analisis2, Definicion, Dirigido, Pregunta, Instrumento) 

# #vector con los nombres de las escalas para filtrar en ins
cod_tipo <- matriz_cfa %>%
  distinct(Cod_indice, .keep_all = T) %>%
  split(.$Analisis2) %>%
  map(~select(.x, Cod_indice)) %>%
  map(~apply(.x, 2, paste, collapse = "|"))

# para pegar al excel
matriz_cod_escalas <- matriz_cfa[c("Concatena1", "Constructo", "Cod_indice", "cod_preg", "Enunciado")]
matriz_constructos <- distinct(matriz_cod_escalas, Concatena1, Cod_indice, Constructo)
matriz_enunciados <- matriz_cod_escalas[c("Concatena1", "cod_preg", "Enunciado")]

## *******************************************************
# (1) acomodarlo en excel (!) [una vaina!]
{
# cfa
ins_cfa <- map(ins, ~keep(.x, stringr::str_detect(names(.x), cod_tipo[[1]]))) #cfa

indicadores_cfa <- map_depth(ins_cfa, 2, 3) %>%
  map_depth(2, 
            ~select(.x, -3) %>%
              pivot_wider(names_from = Indicadores, values_from = Valores)) %>%
  map(~bind_rows(.x, .id = "Variable")) %>%
  bind_rows(.id = "Base") %>%
  left_join(., matriz_constructos, by = c("Variable" = "Cod_indice")) %>%
  select(Base, Variable, Constructo, cfi, tli, srmr, rmsea)

cargas_cfa <- map_depth(ins_cfa, 2, 2) %>%
  map(~bind_rows(.x)) %>%
  bind_rows(.id = "Base") %>%
  left_join(., matriz_enunciados, by = c("Base" = "Concatena1" , "Item" = "cod_preg")) %>%
  select(Base, Variable = Escala, Item, Enunciado, Est, SE, sig.) 

# pca
ins_pca <- map(ins, ~keep(.x, stringr::str_detect(names(.x), cod_tipo[[2]]))) #cfa
ins_pca <- compact(ins_pca)

indicadores_pca <- map_depth(ins_pca, 2, 3) %>%
  map(~map_df(.x, 1) %>% gather) %>%
  bind_rows(.id = "Base") %>%
  left_join(., matriz_constructos, by = c("key" = "Cod_indice")) %>%
  select(Base, Variable = key, Constructo, varianza_explicada = value)

cargas_pca <- map_depth(ins_pca, 2, 2) %>%
  map(~bind_rows(.x, .id = "Variable")) %>%
  bind_rows(.id = "Base") %>%
  left_join(., matriz_enunciados, by = c("Base" = "Concatena1" , "Item" = "cod_preg")) %>%
  select(Base, Variable, Item, Enunciado, Pesos, Cargas) 


export(list("CFA_indicadores de ajuste" = indicadores_cfa,
            "CFA_cargas factoriales" = cargas_cfa,
            "PCA_indicadores" = indicadores_pca, 
            "PCA_cargas" = cargas_pca),
       here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "03-cfa-pca-indicadores.xlsx"))
}

### *************************

# (2) Para el reporte pdf 

# #acomodaciones para los textos
text_por_escala <- matriz_cfa %>%
  mutate(cod_indice3 = ifelse(is.na(Cod_indice2), Cod_indice, Cod_indice2)) %>%
  group_by(Concatena1, Cod_indice) %>%
  #para identificar si tiene mas de un factor
  mutate(nfac = n_distinct(cod_indice3)) %>%
  distinct(Concatena1, cod_indice3, .keep_all = TRUE) %>%
  #texto_2: subescala con definicion:
  mutate(texto_2 = ifelse(nfac > 1, paste0("**", sub_escala, " (",Cod_indice2,")", "**", ": ", Definicion, "\\"), NA)) %>%
  group_by(Concatena1, Cod_indice) %>%
  #juntamos todo el string de texto_2
  mutate(texto_2 = ifelse(nfac > 1, paste0(texto_2, collapse = "\n"), NA)) %>%
  # acomodamos los otros textos:
  distinct(Cod_indice, .keep_all = TRUE) %>%
  mutate(texto = ifelse(nfac > 1,
                        paste0("Los ítems conformaron las siguientes dimensiones:\\","\n\n",texto_2,"\n\n","La pregunta que se realizó al ",Dirigido," fue la siguiente: ","*",Pregunta,"*"),
                        glue::glue("Los ítems hacen referencia a '{Definicion}'. La pregunta que se realizó al {Dirigido} fue la siguiente: *{Pregunta}*")),
         tit_escala = ifelse(nfac > 1, glue::glue("{Constructo}"), glue::glue("{Constructo} ({Cod_indice})")),
         encab_cuest = glue::glue("# {Instrumento}")) %>%
  select(Concatena1, Cod_indice, Analisis2, texto, tit_escala, encab_cuest, nfac, Instrumento)

insumos_reporte <- list(insumos = ins, tipo = cod_tipo, textos = text_por_escala)
save(insumos_reporte, file = here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "03-insumos-para-reporte.Rdata")) #guardamos


### *************************

# (3) Genera los reportes ! 

load(here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "03-insumos-para-reporte.Rdata"))
cuest <- names(insumos_reporte$insumos)
fac <- map(insumos_reporte$insumos, names)

for (i in 1:length(cuest)) { #i=1
  for (j in 1:length(fac[[cuest[i]]])) { #j=3 
    rmarkdown::render(
      envir = new.env(),
      input = here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "04-rmd-generar-reporte-pdf.Rmd"),
      params = list(cuest = cuest[i], fac = fac[[cuest[i]]][[j]]),
      output_file = here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "reportes-cfa-pca", 
                         paste0(cuest[i], "_", fac[[cuest[i]]][[j]], ".pdf"))
    )
  }
}

#se generan archivos .log, borremos... 
unlink(Sys.glob(here("02-reportes-scripts", "02-psicometrico", "01-cfa-pca", "*.log")))

