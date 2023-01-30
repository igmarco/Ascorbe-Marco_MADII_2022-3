# Paquetes
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}

# Importación
ruta_read_rda <- "Datos/Procesados/good_reads_final_preprocesado.rda"
load(file = ruta_read_rda)

# Valores nulos (identificación e imputación)
apply(good_reads, 2, function(x) return(sum(is.na(x))))

good_reads$pages[is.na(good_reads$pages)] <- mean(good_reads$pages, na.rm=TRUE)
good_reads$publish_year[is.na(good_reads$publish_year)] <- median(good_reads$publish_year, na.rm=TRUE)

# Selección de variables numéricas
variables_numericas <- c("author_average_rating",
                         "author_rating_count",
                         "author_review_count",
                         "book_average_rating",
                         "num_ratings",
                         "num_reviews",
                         "pages",
                         "publish_year")

# Para poder mostrar los resultados, evidentemente, necesitamos dos dimensiones
# para conseguirlas vamos a recuperar los resultados obtenidos del apartado
# 2 análisis de componentes principales para proyectar nuestros datos a las dos
# dimensiones.

X <- good_reads[variables_numericas]
df_pca <- prcomp(X, scale. = TRUE)

X_pca <- data.frame(df_pca$x[, 1:2])

# Primero, al igual que vimos en la práctica informática vamos a hacer
# el método iterativo para comprobar como se van recalculando los
# centroides y con ello reagrupando los puntos.

# Mostramos los puntos.

plot(X_pca, pch = 19, col = 2)

# Seleccionamos el número de grupos, primero comenzaremos con k=2 y vamos aumentando.
K <- 2
# Vamos a fijar una semilla
set.seed(123)

# Calculamos los centroides inicales
centroides <- X_pca[sample(dim(X_pca)[1], K), ]
rownames(centroides) <- 1:K
centroides

points(centroides, pch = 4, cex = 4, lwd = 4, col = 3)

# Vemos que los dos centroides han caido entre la "masa" donde se aglomeran
# casi todos los puntos.

# Primera iteración:

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}

head(X_pca)
# asignación de clúster: aquel cuyo centroide esté más cerca
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
head(X_pca)
# actualización de centroides
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
centroides

# Mostramos los resultados:
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)
# Vemos los dos grupos que se han generado, el verde y el rosa. Ahora vamos a ir
# iterando hasta que se estabilicen los clústeres.

# distancias al correspondiente centroide
X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
head(X_pca)
# Comprobamos la varianza total de nuestros clústers
sum(X_pca$dist_centroid^2) # varianza total intra-clústeres
# Vemos que nuestra varianza es alta, vamos a ir iterando y viendo como se reduce.

# Iteración 2:

# Antes de proceder a ella vamos a fijar un epsilon, cuando la resta entre la vaianza total
# de una iteración no supere a este epsilon en la siguiente pararemos.

epsilon = 10
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}

head(X_pca)
# asignación de clúster: aquel cuyo centroide esté más cerca
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
head(X_pca)
# actualización de centroides
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
centroides

# Mostramos los resultados:
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

# distancias al correspondiente centroide
X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
head(X_pca)
# Comprobamos la varianza total de nuestros clústers
sum(X_pca$dist_centroid^2) # varianza total intra-clústeres

# Comprobamos nuestro criterio de parada
var_ant - sum(X_pca$dist_centroid^2) < epsilon
# vemos que no se cumple, seguimos. Aunque a partir de ahora, solo mostraremos
# el gráfico, la varianza total y si se cumple o no la condición.

# Iteración 3:
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon
# Sigue sin cumplirse, así que iteraremos hasta que lo haga.

# Iteración 4:
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon

# Iteración 5:
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon

# Iteración 6:
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon

# Iteración 7:
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon
# Iteración 8
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon

# Iteración 9
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon

#Iteración 10
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon

# Iteración 11
var_ant <- sum(X_pca$dist_centroid^2)

for(i in 1:K){
  txt <- paste("X_pca$distancia",
               i,
               "<- sqrt((X_pca$PC1 - centroides$PC1[",
               i,
               "])^2 + (X_pca$PC2 - centroides$PC2[",
               i,
               "])^2)",
               sep = "")
  eval(parse(text = txt))
}
X_pca$cluster <- apply(X_pca[, 3:(2 + K)], 1, which.min)
centroides <- aggregate(cbind(PC1, PC2) ~ cluster,
                        data = X_pca,
                        mean)
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

X_pca$dist_centroid <- apply(X_pca[, 3:(2 + K)], 1, min)
sum(X_pca$dist_centroid^2)

var_ant - sum(X_pca$dist_centroid^2) < epsilon

# Como podemos comprobar, nuestro criterio de parada se ha cumplido,
# por lo tanto vamos a parar.

# Los grupos generado finales son:
plot(X_pca[, 1:2], pch = 19, col = X_pca$cluster + 1)
points(centroides[, 2:3], pch = 4, cex = 4, lwd = 4,
       col = centroides$cluster + 1)

# Ahora vamos a utilizar la función kmeans y comparar resultados.
datos_km <- kmeans(X_pca[, 1:2], 2, nstart = 1e3)
datos_km
# Calculamos la inercia del modelo
datos_km$tot.withinss
# Vemos que es menor que la que hemos conseguido nosotros de manera artesanal.

# Mostramos los resultados
plot(X_pca[, 1:2], pch = 19, col = datos_km$cluster + 1)
# Vemos que los grupos se han generado dividido por los valores de PC1, es decir,
# los libros con más valoraciones o más populares de los que no lo son.
# Mientras que en nuestro otro caso se dividian por los valores de PC2, dividiendo
# grupos de libros con valoraciones más altas, frente a los que tienen valoraciones
# más bajas.
