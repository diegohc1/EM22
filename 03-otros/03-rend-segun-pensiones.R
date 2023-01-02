

library(here)
library(tidyverse)

show_in_excel <- function(.data){
  tmp <- paste0(tempfile(), ".xlsx") 
  rio::export(.data, tmp)
  browseURL(url = tmp)
} 


# load(here("01-data", "06-ffaa-con-rendimiento", "rend2s.Rdata"))
em2s22 <- rio::import(here("01-data", "05-bd-rendimiento", "EM_2S_2022_todo_P_eqp_con_corte.sav"))
pension19 <- rio::import("D:/1. UMC/2019/bases/BD_bajocosto_secundaria_2019.csv")
ece2s19  <- rio::import("D:/1. UMC/2019/bases/ECE 2S 2019 estudiantes.sav")

# agregar 0 a codmod7 en pension19 -_-
pension19 <- mutate(pension19, cod_mod7 = str_pad(cod_mod7, 7, pad = "0"))
table(pension19$anexo)
pension19 <- mutate(pension19, estrat03 = ifelse(estrat03 == "P\xfablica", "Pública", estrat03)) 
unique(pension19$estrat03)

lev <- c("Rural", "Pública", "Bajo costo", "Medio costo", "Alto costo", "Muy alto costo")
pension19 <- mutate(pension19, estrat03 = factor(estrat03, levels = lev, labels = lev))

# pegamos! 
em2s22 <- left_join(em2s22, pension19, by = "cod_mod7")
ece2s19 <- left_join(ece2s19, pension19, by = "cod_mod7")

# solo Lima y Callao 
unique(ece2s19$nom_dre)
ece2s19 <- filter(ece2s19, nom_dre %in% c("Lima Metropolitana", "Callao"))
em2s22 <- filter(em2s22, Provincia %in% c("LIMA", "CALLAO"))

# rendimiento segun pension19
names(ece2s19)
names(em2s22)
unique(em2s22$Provincia)
unique(ece2s19$estrat03)

tab1 <- bind_rows(
  
  ece2s19 %>%
    group_by(estrat03) %>%
    factorito::mean_prop_grupo("M500_CN") %>%
    mutate(a = "2019"),
  
  em2s22 %>%
    drop_na(Peso_CN) %>%
    group_by(estrat03) %>%
    factorito::mean_prop_grupo("M500_EM_2S_2022_CN", w = "Peso_CN") %>%
    mutate(a = "2022")

)

tab1 <- tab1 %>% filter(estrat03 != "Rural")

ggplot(tab1, aes(x = estrat03, y = media, color = a)) + 
  geom_point(size = 4.5) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.position = "bottom") + 
  labs(x = "\nTipología escuelas privadas", 
       y = "Rendimiento promedio\n") + 
  geom_text(aes(label = format(round(media), decimal.mark = ",")), 
            hjust = -0.3, size = 5, color = "black")

dev.off()

tab2 <- bind_cols(
  
  ece2s19 %>%
    group_by(estrat03) %>%
    factorito::mean_prop_grupo("M500_CN") %>%
    select(estrat03, media19 = media),
  
  em2s22 %>%
    drop_na(Peso_CN) %>%
    group_by(estrat03) %>%
    factorito::mean_prop_grupo("M500_EM_2S_2022_CN", w = "Peso_CN") %>%
    select(media22 = media)
)

tab2 %>%
  mutate(dif = media22 - media19) %>%
  show_in_excel()


