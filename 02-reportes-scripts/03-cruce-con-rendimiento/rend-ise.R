
rm(list = ls())

devtools::source_url("https://raw.githubusercontent.com/diegohc1/para_funciones/main/funciones/0-funciones-nuevas-22.R")
load(here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))

show_in_excel <- function(.data){
  tmp <- paste0(tempfile(), ".xlsx") 
  rio::export(.data, tmp)
  browseURL(url = tmp)
} 

bd <- rend2sl$EM2022_2Sestudiante_EBRD1
names(bd)



bd1 <- bd %>%
  select(cod_mod7.x, starts_with("M500"), ise2S, starts_with("Peso")) %>%
  mutate(qise = ntile(ise2S, 4)) 

vrend <- names(select(bd1, starts_with("M500")))
vpeso <- c("Peso_lectura", "Peso_mate", "Peso_CN")
vrend2 <- c("lec", "mat", "cyt")

tab1 <- list()
for(i in 1:3){ #i=1
  
  bd_i <- drop_na(bd1, .data[[vpeso[[i]]]])
  
  tab1[[i]] <- bd_i %>%
    group_by(qise) %>%
    factorito::mean_prop_grupo(m = vrend[[i]], w = vpeso[[i]]) %>%
    mutate(rend = vrend2[[i]])
  
}

aa <- bind_rows(tab1)

show_in_excel(aa)

bd1 %>%
  group_by(qise) %>%
  mean_prop_estrato(medida = M500_EM_2S_2022_MA, peso = Peso_mate)

