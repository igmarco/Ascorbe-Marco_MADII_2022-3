# Definición de funciones auxiliares.

asim <- function(x){
  x <- x[!is.na(x)]
  n <- length(x)
  return(mean((x - mean(x))^3)/(sd(x)^3))
}

getYear <- function(dateString){
  return(unlist(strsplit(dateString, " "))[length(unlist(strsplit(dateString, " ")))] )
}

mode <- function(x) {
  x <- x[!is.na(x)]
  return(names(which.max(table(x))))
}

mcov <- function(X){
  n <- dim(X)[1]
  return(cov(X)*(n - 1)/n)
}
