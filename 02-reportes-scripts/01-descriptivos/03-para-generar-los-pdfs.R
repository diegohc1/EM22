rm(list=ls())


# reporte por cuestionario -----

load(file = here("02-reportes-scripts", "01-descriptivos", "01-descriptivos-ffaa.Rdata"))
cuest <- unique(tab_final2$Concatena1)

for (i in 1:length(cuest)) { #i=1 
  rmarkdown::render(
    input = here("02-reportes-scripts", "01-descriptivos", "03-genera-reporte-pdf.Rmd"),
    params = list(cuest = cuest[i]),
    output_file = here("02-reportes-scripts", "01-descriptivos", "reportes", "00-nacional", paste0(cuest[i], ".pdf"))
    )
}


# reporte por region ----- 
# aun no esta! 
load(file = here("4-descriptivos", "01-descriptivos-ffaa.Rdata"))
cuest <- unique(tab_final2$Concatena2)
tabla_reg <- tab_final2 %>% filter(tipo == "Region") 
region <- unique(tabla_reg$estrato)[1:4]

for (r in 1:length(region)) {  #r=1
  for(i in 1:length(cuest)){ #i=1
  rmarkdown::render(
    input = here("4-descriptivos", "03-estructura-reporte-regiones.Rmd"),
    params = list(region = region[r], cuest = cuest[i]),
    output_file = here("4-descriptivos", "03-reportes", paste0(region[r], "_", cuest[i], ".pdf"))
  )
}
}





#intento purrr
# reportes <- tibble::tibble(
#   input = here("3-reportes", "1-descriptivos", "02-genera-reporte-varios.Rmd"),
#   output_file = here("3-reportes", "1-descriptivos", "1-reporte-por-cuest", paste0(cuest, ".pdf")),
#   params = map(cuest, ~list(cuest = .))
#   )
# 
# reportes %>% pwalk(rmarkdown::render)
# sale mal!

 

