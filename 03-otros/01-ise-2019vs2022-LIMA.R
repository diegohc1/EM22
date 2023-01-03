

library(tidyverse)

bd <- rio::import("C:/Users/factoresasociados03/Desktop/explora-ise-19vs22-lima.xlsx")
bd

bd1 <- pivot_longer(bd, cols = starts_with("Cargas"), names_to = "Operativo", names_prefix = "Cargas ", values_to = "Cargas")
it <- unique(bd1$Item2)
bd1 <- mutate(bd1, Item2 = factor(Item2, levels = it, labels = it))

bd1 %>%
  ggplot(aes(x = fct_rev(Item2), y = Cargas, color = Operativo)) + 
  geom_point(size = 2) +
  coord_flip(y = c(0.10, 1)) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) + 
  labs(x = "", y = "Cargas en el primer componente") + 
  geom_vline(xintercept = 5.5) + 
  geom_vline(xintercept = 19.5) + 
  geom_vline(xintercept = 22.5)  
  
