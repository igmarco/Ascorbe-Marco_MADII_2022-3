# Paquetes
if(!require(readr)){
  install.packages("readr", dependencies = TRUE, repos='http://cran.rstudio.com/')
  require(readr)
}
if(!require(psych)){
  install.packages("psych")
  require(psych)
}
if(!require(matrixStats)){
  install.packages("matrixStats")
  require(matrixStats)
}

if(!require(car)){
  install.packages("car")
  require(car)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(GGally)){
  install.packages("GGally")
  require(GGally)
}
# if(!require(patchwork)){
#   install.packages("patchwork")
#   require(patchwork)
# }

# Importación
ruta_read_rda <- "Datos/Procesados/good_reads_final_preprocesado.rda"
load(file = ruta_read_rda)

# Resúmenes
variables_categoricas <- c("author_gender",
                           "author_genres",
                           "birthplace",
                           "genre_1",
                           "genre_2")
variables_numericas <- c("author_average_rating",
                         "author_rating_count",
                         "author_review_count",
                         "book_average_rating",
                         "num_ratings",
                         "num_reviews",
                         "pages",
                         "publish_year",
                         "score")

colnames(good_reads)
head(good_reads, 5)

summary(subset(good_reads, select = -c(author_genres)))

# Estudio de las variables numéricas
# Obtención de estadísticos básicos
colMeans(good_reads[variables_numericas], na.rm=TRUE)
apply(good_reads[variables_numericas], 2, sd, na.rm=TRUE)
apply(good_reads[variables_numericas], 2, asim)

# Resúmenes detallados
describe(good_reads[variables_numericas])


# Estudio de las variables categóricas
unique(good_reads$author_gender)
unique(good_reads$birthplace)
unique(good_reads$genre_1)
unique(good_reads$genre_2)

# Subconjuntos
unique(good_reads$author_gender)

good_reads_female <- subset(good_reads,
                      author_gender == 'female',
                      -author_gender)
head(good_reads_female)

good_reads_male <- subset(good_reads,
                          author_gender == 'male',
                          -author_gender)
head(good_reads_male)

# Comparación de estadísticos de los subconjuntos
colMeans(good_reads_female[variables_numericas], na.rm=TRUE)
colMeans(good_reads_male[variables_numericas], na.rm=TRUE)

apply(good_reads_female[variables_numericas], 2, sd, na.rm=TRUE)
apply(good_reads_male[variables_numericas], 2, sd, na.rm=TRUE)

apply(good_reads_female[variables_numericas], 2, asim)
apply(good_reads_male[variables_numericas], 2, asim)


# Con aggregate
aggregate(x = score ~ author_gender,
          data = good_reads,
          FUN = mean, na.rm=TRUE)

aggregate(x = score ~ author_gender + genre_1,
          data = good_reads,
          FUN = mean, na.rm=TRUE)

aggregate(x = score ~ author_gender + birthplace,
          data = good_reads,
          FUN = length)

# El coeficiente de asimetría negativo más una baja SD nos indica que, para 
# España, la mayor parte de los libros que toma son de antes del 2000.
aggregate(x = publish_year ~ birthplace,
          data = good_reads,
          FUN = function(x) return(c(mean(x, na.rm=TRUE), 
                                     sd(x, na.rm=TRUE), 
                                     asim(x))))

# Valores nulos (identificación e imputación)
apply(good_reads, 2, function(x) return(sum(is.na(x))))

good_reads$birthplace[is.na(good_reads$birthplace)] <- mode(good_reads$birthplace)
good_reads$pages[is.na(good_reads$pages)] <- mean(good_reads$pages, na.rm=TRUE)
good_reads$publish_year[is.na(good_reads$publish_year)] <- median(good_reads$publish_year, na.rm=TRUE)

# Matriz de varianzas-covarianzas
cov(good_reads[variables_numericas]) # cuasivarianzas-Cuasicovarianzas
mcov(good_reads[variables_numericas])

# Matriz de correlaciones
cor(good_reads[variables_numericas])




# Visualización
# Sampleo inicial
sample_good_reads <- sample_frac(good_reads, size = .01, replace = F)
sample_good_reads <- sample_good_reads[c("author_gender", variables_numericas)]
sample_good_reads_catGender <- sample_good_reads
head(sample_good_reads_catGender)

sample_good_reads$author_gender <- ifelse(sample_good_reads$author_gender == "female", 0, 1)
head(sample_good_reads)

# Representación general
scatterplotMatrix(sample_good_reads, pch = 19, smooth = FALSE, regLine = FALSE)

ggpairs(sample_good_reads_catGender, 
        columns=c("author_average_rating",
                  "author_rating_count",
                  "author_review_count"),
        mapping=aes(color=author_gender))

ggpairs(sample_good_reads_catGender, 
        columns=c("book_average_rating",
                   "num_ratings",
                   "num_reviews",
                   "pages",
                   "publish_year",
                   "score"),
        mapping=aes(color=author_gender))
                  

# Vale, pues queda hacer gráficos más específicos y quizá extraer información
# con el GGally para submuestras con los 3 países y los 5 géneros más populares.