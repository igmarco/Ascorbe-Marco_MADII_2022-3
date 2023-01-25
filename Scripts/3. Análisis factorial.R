# Paquetes
if(!require(psych)){
  install.packages("psych")
  require(psych)
}

# Importación
ruta_read_rda <- "Datos/Procesados/good_reads_final_preprocesado.rda"
load(file = ruta_read_rda)

# Valores nulos (identificación e imputación)
apply(good_reads, 2, function(x) return(sum(is.na(x))))

good_reads$birthplace[is.na(good_reads$birthplace)] <- mode(good_reads$birthplace)
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

# Análisis factorial "a mano".
p <- ncol(X)
R <- cor(X)
R

Rprop <- eigen(R)
Rprop

Rprop$vectors %*% diag(Rprop$values) %*% t(Rprop$vectors) # = R

# Porcentajes acumulados de varianza
cumvar <- 100*cumsum(Rprop$values)/p

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
L

# La primera variable habla de la popularidad y la segunda del nivel.

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

ggplot(good_reads, aes(x = 1:p, y = h2, fill=1:p)) +
  geom_bar()

# Estimación de la matriz de correlaciones
Rbarra <- L %*% t(L) + diag(psi)
Rbarra








