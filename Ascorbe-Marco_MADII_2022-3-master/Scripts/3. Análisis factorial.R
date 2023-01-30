# Paquetes
if(!require(psych)){
  install.packages("psych")
  require(psych)
}
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

X <- good_reads[variables_numericas]

# Análisis factorial "Artesanal"
# Conseguimos el número de variables y la matriz de correlaciones de nuestros
# datos.
p <- ncol(X)
R <- cor(X)
R

# Calculamos los valores y vectores propios
Rprop <- eigen(R)
Rprop

Rprop$vectors %*% diag(Rprop$values) %*% t(Rprop$vectors) # = R

# Porcentajes acumulados de varianza
cumvar <- 100*cumsum(Rprop$values)/p

# Método del codo para decidir cuantos factores nos quedamos.
plot(
  1:p,
  100 - cumvar,
  type = "b",
  pch = 19,
  xlab = "Variables",
  ylab = "Varianza acumulada"
)

# Elegimos 2 factores y obtenemos la estimación de la matriz de cargas
m <- 2
sum(Rprop$values[(m + 1):p]) # cota de la suma de cuadrados de la matriz residual
L <- (rep(1, p) %*% t(sqrt(Rprop$values[1:m]))) * Rprop$vectors[, 1:m]
rownames(L) <- colnames(X)
L
# La primera variable habla de la popularidad y la segunda del nivel. Donde en la
# primera vemos que todas las variables relevantes son el número de reseñas y valoraciones
# tanto como para el autor como para la propia obra. Y la segunda, vemos que las dos variables
# más importantes hacen referencia a la puntuación media tanto del autor como de la obra.


# Comunalidades
h2 <- rowSums(L^2)
h2

plot(
  1:p,
  h2,
  pch = 19,
  xlab = "Variables",
  ylab = "Comunalidad"
)

# Vemos que las dos últimas variables "pages" y "publish_year" tienen una comunalidad
# prácticamente nula, y probablemente una unicidad muy elevada, eso lo veremos más adelante.
# El resto de variables tienen la comunalidad bastante alta, siendo las que más tienen
# "author_average_rating" y "book_average_rating".

# Unicidades
psi <- 1 - h2
psi
# diag(S) - h2 con S <- vcov(X)

plot(
  1:p,
  psi,
  pch = 19,
  xlab = "Variables",
  ylab = "unicidad"
)

# Como comentábamos antes, los valores se han intercambiado. En resumen, el modelo
# está bien ajustado, solo que dos variables se quedan bastante fuera. Recordemos que
# debemos tener un equilibrio entre tener poca unicidad y pocos factores. Lo suyo sería
# ver qué pasa si añadimos un tercer factor, pero eso es mejor que lo probemos cuando usemos
# la función "principal". 


ggplot(data.frame(cbind(1:p, h2)), aes(x = V1, y = h2, fill=1:p)) +
  geom_bar(stat='identity')

# Vemos en este diagrama de barras, las seis variables con alta comunialidad
# y las dos variables con baja comunalidad.

# Estimación de la matriz de correlaciones para posteriormente calcular los residuos.
Rbarra <- L %*% t(L) + diag(psi)
Rbarra

# Calculamos la matriz residual y a partir de ella su suma de cuadrados
# para tener una medida de calidad de ajuste.
D <- R - Rbarra
sum(D^2)

# Ahora vamos a comprobar que efectivamente se cumple que esta medida es menor
# que la suma de los 8 valores porpios de nuestra matriz

sum(D^2) <= sum(Rprop$values[3:p])

# Vemos que se cumple y por ello, nuestra elección de dos factores es acertada.
# si solo hubiesemos cogido uno puede que esto no se cumplíese.


# Ahora vamos a mostrar gráficamente las distintas cargas en el espacio de los factores
L <- as.data.frame(L)
rownames(L) <- colnames(X)
colnames(L) <- c("F1", "F2")
ggplot(L, aes(x = F1, y = F2, label = rownames(L))) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_text()

# Vemos que hay dos variables que se quedan en tierra de nadie, tanto publish_year,
# como pages. Y que las variables que comentabamos anteriormente son las que determinan cada
# uno de los factores, dándonos información sobre popularidad (F1) y sobre nivel o valoración (F2).

# Aplicamos la rotación para maximizar la varianza total.
vmax_L <- varimax(as.matrix(L))
# Y comprobamos los valores rotados
Lstar <- as.data.frame(vmax_L$loadings[, 1:m])
rownames(Lstar) <- colnames(X)
colnames(Lstar) <- c("F1", "F2")
ggplot(Lstar, aes(x = F1, y = F2, label = rownames(Lstar))) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_text()

# Vemos que las cargas se han ajustado aún más a los ejes de cada factor y nuestras
# variables con alta unicidad poco siguen aportando.

# Ahora, vamos a aplicar la función principal para hacer todo lo anterior de una manera
# mucho más cómoda y sencilla.

# Antes cargamos los datos en X para mayor completitud.
X <- good_reads[variables_numericas]

# Comenzamos como antes con dos factores y sin rotación.
mt_af <- principal(X, nfactors = 2, rotate = "none")
mt_af

mt_af$values
mt_af$loadings
mt_af$communality
# amos a volver a mostrar las comunalidades y ver si las dos variables que tenían
# baja comunlaidad efectivamente mejorar si aumentamos el número de factores.
ggplot(data.frame(cbind(1:p, mt_af$communality)), aes(x = X1, y = X2, fill=1:p)) +
  geom_bar(stat='identity')

# Vemos que estas dos variables: año de publicación y número de páginas vuelven a 
# tener poca comunalidad. De nuevo, vamos a aumentar el número de factores en pos de 
# que mejoren.
mt_af_3 <- principal(X, nfactors = 3, rotate = "none")
ggplot(data.frame(cbind(1:p, mt_af_3$communality)), aes(x = X1, y = X2, fill=1:p)) +
  geom_bar(stat='identity')
# Como podemos comprobar, ahora sí que tenemos bajas unicidades, el número de páginas
# sigue teniendo una comunalidad baja, pero en general hemos mejorado estos números.
# Podríamos seguir aumentando el número de factores y con ello mejorarían las comunalidades
# pero esto conllevaría un desequilibrío, ya que nuestro objetivo es minimizar el número de éstos.
mt_af_4 <- principal(X, nfactors = 4, rotate = "none")
ggplot(data.frame(cbind(1:p, mt_af_4$communality)), aes(x = X1, y = X2, fill=1:p)) +
  geom_bar(stat='identity')
# Como podemos comprobar, con 4 factores, ninguna variable tiene la comunalidad baja
# pero decidimos quedarnos con 3 para tratar de encontrar un equilibrio.

# Como hemos decidido quedarnos con tres factores, vamos a mostrar de nuevo para
# estos, los valores de cada variable:

mt_af_3$loadings

# Vemos que el primer factor hace referencia de nuevo a la popularidad, el segundo a
# el nivel o aprobación media y el tercero a libros con pocas páginas y recientes, que puede
# tener sentido para encontrar aquellos libros que son más o menos modernos y que no tienen
# una barbaridad de páginas.

# Ahora comprobemos la complejidad de Hoffman para cada variables, de modo que búscamos
# que cada una tenga un número cercano a cero.

# Manera directa
mt_af_3$complexity

# Manera artesanal
indice_Hoffman <- function(x) (sum(x^2)^2)/sum(x^4)
comp <- apply(mt_af_3$loadings, 1, indice_Hoffman)
comp

# Calculamos la complejidad media.
mean(comp)

# Como vemos, tenemos una complejidad media de 1, lo cual es casi perfecto. Y en general,
# vemos que casi todas las variables tienen una complejidad cercana a 1, las más altas son:
# pages y author_average_rating.

# Vemos como aportan cada una de las variables a cada factor con la siguiente función.
fa.diagram(mt_af_3)

# Y ya por último, vamos a mostrar las comunalidades pero aplicando la rotación varimax
mt_af_3R <- principal(X, nfactors = 3)
mt_af_3R
ggplot(data.frame(cbind(1:p, mt_af_3R$communality)), aes(x = X1, y = X2, fill=1:p)) +
  geom_bar(stat='identity')

# Vemos que prácticamente no varia al aplicar esta rotación. Mostramos el digrama de
# mayor carga para cada variable
fa.diagram(mt_af_3R)

# Vemos que no cambia prácticamente nada.

# Como último apartado, vamos a comprobar los residuos de un análisis con una
# sola componente, para ver si es suficiente o no.
L_1 <- (rep(1, p) %*% t(sqrt(Rprop$values[1:1]))) * Rprop$vectors[, 1:1]
h2_1 <- rowSums(L_1^2)
psi_1 <- 1 - h2_1

D_1 <- R - (L_1 %*%t(L_1) + diag(psi_1))
sum(D_1^2)
# Vemos el error para dos factores
sum(D^2)
# Como podemos comprobar, efectivamente no sería un buen ajuste, ya que la hemos
# cogido un solo factor y empeora mucho el error, con residuos muy altos.