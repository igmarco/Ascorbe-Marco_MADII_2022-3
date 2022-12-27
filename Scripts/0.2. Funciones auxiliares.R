# Definición de funciones auxiliares.

asim <- function(x){
  n <- length(x)
  return(mean((x - mean(x))^3)/(sd(x)^3))
}

getYear <- function(dateString){
  return(unlist(strsplit(dateString, " "))[length(unlist(strsplit(dateString, " ")))] )
}
