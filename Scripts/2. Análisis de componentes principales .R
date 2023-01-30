if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

# Importación
ruta_read_rda <- "Datos/Procesados/good_reads_final_preprocesado.rda"
load(file = ruta_read_rda)
head(good_reads, 10)
# Recordemos que teniamos valores nulos, visto en el apartado anterior
# así que en este caso los volvemos a imputar
good_reads$pages[is.na(good_reads$pages)] <- mean(good_reads$pages, na.rm=TRUE)
good_reads$publish_year[is.na(good_reads$publish_year)] <- median(good_reads$publish_year, na.rm=TRUE)

# Tenemos variables categóricas y numéricas, para hacer
# este análisis vamos a quedarnos con las numéricas y usar el género principal
# para visualizar mejor los resultados, de entre todos los géneros nos quedaremos con los cinco más
# populares.

#Nos quedamos con las variables deseadas y las guardamos en un dataframe
variables_numericas <- c("author_average_rating",
                         "author_rating_count",
                         "author_review_count",
                         "book_average_rating",
                         "num_ratings",
                         "num_reviews",
                         "pages",
                         "publish_year")

good_reads_genres <- good_reads %>% group_by(genre_1)
good_reads_genres_count <- good_reads_genres %>% summarise(copies = n())

mostCommonGenres <- head(good_reads_genres_count[order(good_reads_genres_count$copies, decreasing=TRUE),1:2],5)

good_reads_mostCommonGenres <- filter(good_reads_genres, 
                                      any(mostCommonGenres$genre_1 == genre_1))

good_reads_mostCommonGenres$genres <- paste(good_reads_mostCommonGenres$genre_1)

df_pca <- good_reads_mostCommonGenres[,c(variables_numericas, "genres")]

# Separamos en X e y para el análisis
X <- as.matrix(df_pca[, -9])

# Primero, vamos a hacer el análisis de componentes principales de
# manera "artesanal", así como vimos en 
# la práctica de la asignatura por mayor completitud. Sabemos que no es necesario
# pero sí interesante.
p <- ncol(X)
S <- cov(X)
# Calculamos la varianza total
var_tot <- sum(diag(S))
var_tot

# Calculamos los valores y vectores propios
S_vvprop <- eigen(S)
sum(S_vvprop$values)
# Vemos que corresponde con la suma de las varianzas de nuestras variables
100*cumsum(S_vvprop$values)/var_tot
# Con tan solo las dos primeras componentes llegamos al 99% de varianza acumulada
# aún así vamos a aplicar el método del codo para comprobarlo
plot(1:p,
     S_vvprop$values,
     type = "b",
     pch = 19,
     xlab = "#CP",
     ylab = "Varianza")
# Es evidente que son dos componentes, con una serviría realmente,
# pero es preferible tener un par de componentes.
k <- 2
X_star <- scale(X, scale = FALSE)

# Calculamos las dos componentes principales y las guardamos en la matriz
# Y
Y <- X_star %*% S_vvprop$vectors[, 1:k]
summary(Y)
head(Y)
cov(Y)
S_vvprop$vectors[, 1:k]
# Vemos los pesos que tienen cada una de las variables en las 2 componentes
# comprobamos que las que más influyen son el número de reseñas y valoraciones
# tanto para los libros y los autores.
cor(X, Y)

# Vamos a mostrar los resultados
df_pca <- cbind(df_pca, Y)
colnames(df_pca)[10:11] <- paste("CP", 1:k, sep = "")
head(df_pca)
ggplot(df_pca, aes(x = CP1, y = CP2, color = genres, fill = genres)) + geom_point()
ggplot(df_pca, aes(x = CP1, y = genres)) + geom_point()
ggplot(df_pca, aes(x = CP2, y = genres)) + geom_point()

# En la primera figura no llegamos a ver nada realmente útil, pero si que encontramos
# algunos géneros distanciados del resto. Serán obras muy populares, con muchas reseñas
# y valoraciones. El resto está aglomerado en un cono y es dificil ver nada. Esto tanto
# para la primera componente, como para la segunda.

# En las otras dos figuras vemos como influyen las distintas componentes a los géneros.
# sí que se pueden llegar a apreciar que generos como fantasía, ficción y jóvenes adultos destacan
# por tener valores muy alejados del resto, es decir, muy populares. Otros géneros, según
# este dataset, no destacan tanto, como las novelas históricas o el romance contemporáneo.

# Pasamos a usar prcomp que es la librería pensada para hacer estos análisis.
df_acp_cov <- prcomp(X)
df_acp_cov
# Podemos ver que el resultados obtenido anteriormente es ligeramente distinto a este
plot(df_acp_cov, type = "l", pch = 19)
# De nuevo, parece casi obligatorio quedarse con k = 2.
cov(df_acp_cov$x[, 1:2])
df_acp_cov$rotation[, 1:2]
# Vemos qué características ingluyen más en cada componente
# y que en general la hegemonía de las valoraciones sobre las demás.
# Recuperamos el dataset original y le añadimos las dos componentes
df_pca <- good_reads_mostCommonGenres[,c(variables_numericas, "genres")]
df_pca <- cbind(df_pca, df_acp_cov$x[, 1:2])
# Hecho esto vamos a visualizar los resultados
ggplot(df_pca, aes(x = PC1, y = PC2, color = genres)) + geom_point()
ggplot(df_pca, aes(x = PC1, y = genres)) + geom_point()
# Vemos que los gráficos generados son prácticamente identicos a los obtenidos por el
# método artesanal. Sí que en el primero, se pueden apreciar grupos divididos por valores
# distintos en el eje de la primera componente, paralelos entre sí.
biplot(df_acp_cov, xlabs = rep("·", nrow(df_pca)))
# En esta gráfica, lo relevante son los ejes y las flechas, más que las observaciones,
# y es que solo dos variables tienen relevancia, "rating_count" para PC1 y "num_ratings"
# para PC2.

# Por terminar con este apartado, vamos a utilizar la matriz de correlaciones
# y no la de covarianzas para realizar el análisis.
df_acp_corr <- prcomp(X, scale. = TRUE)
df_acp_corr
summary(df_acp_corr)
plot(df_acp_corr, type = "l", pch = 19)
# Aquí si que es más complicado decidirse, y podría paracer que lo mejor es
# k=6, bueno una menos porque el cambio se produce en el 5. Pero por sencillez
# cogeremos k = 2.
df_pca <- good_reads_mostCommonGenres[,c(variables_numericas, "genres")]
df_pca <- cbind(df_pca, df_acp_corr$x[, 1:2])

ggplot(df_pca, aes(x = PC1, y = PC2, color = genres, fill = genres)) +
  geom_point()
# Realmente no se puede sacar demasiado en claso de esta gráfica, solo que se nota
# que nuestras variables numéricas no pueden ser negativas y son cortades en ese eje.
# No obstante, se pueden apreciar como tendencias de los géneros, pocos puntos veres o azules
# (historicos y romance en ese orden) se llegan a distanciar del conglomerado, mientras
# que hay aglomeraciones estiradas paralelas de generos destacados, como fantasía
# o ficción.