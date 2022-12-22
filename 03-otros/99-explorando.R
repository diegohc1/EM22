
# explorando algunos codigos que pueden servir 


# trycatch ----- 

l <- list(1, 2, "a", 3, -1, 100)

# para el loop 
g <- list()
for(i in 1:length(l)){ #i=3
  g[[i]] <- log(l[[i]])
}

# sigue el loop 
g <- list()
for(i in 1:length(l)){
  
  tryCatch({g[[i]] <- log(l[[i]])}, error = function(e) print(e$message))
  
}


# filter/mutate segun condicion de varias columnas ----
df <- data.frame(
  p01_1 = c(1, 1, 2, 2),
  p01_2 = c(1, 1, 2, 2),
  p01_3 = c(2, 1, 1, 2),
  p01_4 = c(1, 1, 1, 1)
)

filter(df, if_any(everything(), ~.x == 1)) # si de todos, alguno es 1 
filter(df, if_any(c("p01_1", "p01_2"), ~.x == 1)) # si de p01_1 y p01_2, alguno es 1 
filter(df, if_all(everything(), ~.x == 1)) # si de todos, todos es 1 
filter(df, if_all(c("p01_1", "p01_2"), ~.x == 1)) # si de p01_1 y p01_2, todos es 1 

mutate(df, id = ifelse(if_all(everything(), ~.x == 1), 1, 0)) # si de todos, todo es 1
mutate(df, id = ifelse(if_all(c("p01_1", "p01_2", "p01_3"), ~.x == 2), 1, 0)) # si de todos, todo es 2
mutate(df, id = ifelse(if_any(everything(), ~.x == 1), 1, 0)) # si de todos, alguno es 1

# ???
mutate(df, id = ifelse(if_all(c("p01_1", "p01_2", "p01_3"), ~.x == 2) & "p01_4" == 1 , 1, 0)) # (?)
filter(df, if_all(c("p01_1", "p01_2", "p01_3"), ~.x == 1) & "p01_4" == 1) 
