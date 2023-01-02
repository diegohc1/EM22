# algunas funciones de apoyo 

# para verificar que los elementos de la matriz y la lista de bases son iguales
matriz_vs_lista <- function(x1, x2){
  mismos0 <- length(x1) == length(x2)
  mismos1 <- setequal(names(x1), names(x2))
  mismos2 <- identical(names(x1), names(x2))
  
  return(
    cat(
      "\n", "Misma cantidad de elementos:", mismos0, "\n",
      "Mismos elementos (nombres) sin importar el orden:", mismos1, "\n",
      "Exactamente los mismos elementos (nombres):", mismos2, "\n")
  )
}


# para darle una inspeccion a las correlaciones
chequeo <- function(mc){
  
  c1 <- apply(mc, 1, function(x) all(x[x != 1] > 0)) #todas las correlaciones son positivas?
  c2 <- apply(mc, 1, function(x) any(x[x != 1] > 0)) #hay algun positivo?
  c3 <- apply(mc, 1, function(x) any(x[x != 1] > 0.30)) #todas las correlaciones son mayores a .30?
  
  tabla1 <- data.frame(
    item = names(c1),
    todo_positivo = c1,
    hay_positivo = c2,
    todos_min_30 = c3,
    KMO_item = psych::KMO(mc)$MSAi,
    KMO = psych::KMO(mc)$MSA)
  
  return(tabla1)
  
}

# para ver si hay duplicados segun algun id 
dupli <- function(data, id){
  nr <- data.frame(table(data[[id]]))
  idrep <- nr[nr$Freq > 1,]
  cuantosrep <- table(nr$Freq)
  return(list(id = idrep, freq = cuantosrep))
}


#***********************************************************************************************
#function para aplicar a todos
library(openxlsx)

crear_hojas_tablas <- function(listatab, preg, enun, file){
  
  #creamos 
  wb <- createWorkbook() 
  
  #nombres de las hojas
  sheetnam <- names(listatab)
  
  #crear hojas
  walk(sheetnam, ~addWorksheet(wb, .x))
  
  #añadir a cada hoja
  walk2(sheetnam, preg, ~writeData(wb, sheet = .x, .y, startRow = 1)) #preguntas
  walk2(sheetnam, enun, ~writeData(wb, sheet = .x, .y, startRow = 2)) #enunciados
  walk2(sheetnam, listatab, ~writeData(wb, .x, .y, startCol = 1, startRow = 3, rowNames = FALSE)) #tabla
  
  #estilos
  estilo_preg <- createStyle(fontColour = "#1F497D", textDecoration = "bold")
  estilo_enun <- createStyle(fontColour = "#1F497D")
  estilo_encab_tabla <- createStyle(textDecoration = "bold", halign = "center", border = "TopBottom")
  est_cuerpo_tabla <- createStyle(halign = "center")
  est_chiqui <- createStyle(fontColour = "#FFFFFF", border = "TopBottom")
  
  #aplicar estilos a todas las hojas
  walk(sheetnam, ~addStyle(wb, .x, style = estilo_preg, rows = 1, cols = 1)) #pregunta
  walk(sheetnam, ~addStyle(wb, .x, style = estilo_enun, rows = 2, cols = 1)) #enunciado
  walk2(sheetnam, listatab, ~addStyle(wb, .x, style = estilo_encab_tabla, rows = 3, cols = 2:ncol(.y))) #encabtabla
  walk2(sheetnam, listatab, ~addStyle(wb, .x, style = est_cuerpo_tabla,  rows = 4:(nrow(.y)+3), cols = 2:ncol(.y), gridExpand = TRUE))
  walk(sheetnam, ~addStyle(wb, .x, style = est_chiqui, rows = 3, cols = 1)) #para darle un toque
  
  #ancho columna
  walk2(sheetnam, listatab, ~setColWidths(wb, .x,  cols = 2:ncol(.y), widths = 26.29))
  walk(sheetnam, ~setColWidths(wb, .x,  cols = 1, widths = 18))
  
  # Save the workbook
  saveWorkbook(wb, file, overwrite = T)
  
}

# #contar NA en las observaciones
rowSumsNA <- function(x) rowSums(is.na(x)) 

# invertir escalas
invertir <- function(x, i) i+1 - x 

