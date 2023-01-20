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
                         "publish_year")

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
aggregate(x = book_average_rating ~ author_gender,
          data = good_reads,
          FUN = mean, na.rm=TRUE)

aggregate(x = book_average_rating ~ author_gender + genre_1,
          data = good_reads,
          FUN = mean, na.rm=TRUE)

aggregate(x = book_average_rating ~ author_gender + birthplace,
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
                   "publish_year"),
        mapping=aes(color=author_gender))
                  
# Representación de histogramas
ggplot(sample_good_reads_catGender, 
       aes(x = pages)) + 
  geom_histogram()

ggplot(sample_good_reads_catGender, 
       aes(x = author_average_rating, 
           fill = author_gender)) + 
  geom_histogram()

ggplot(sample_good_reads_catGender, 
       aes(x = author_rating_count, 
           fill = author_gender)) + 
  geom_histogram()

# Diagramas de cajas
# Vamos a quedarnos con los géneros con más ejemplares.
good_reads_genres <- good_reads %>% group_by(genre_1,genre_2)
good_reads_genres_count <- good_reads_genres %>% summarise(copies = n())

mostCommonGenres <- head(good_reads_genres_count[order(good_reads_genres_count$copies, decreasing=TRUE),1:2],20)

good_reads_mostCommonGenres <- filter(good_reads_genres, 
                                      any(mostCommonGenres$genre_1 == genre_1 & 
                                            mostCommonGenres$genre_2 == genre_2))

good_reads_mostCommonGenres$genres <- paste(good_reads_mostCommonGenres$genre_1, 
                                            '-', 
                                            good_reads_mostCommonGenres$genre_2)

# Vamos a quedarnos con los lugares de nacimiento con más ejemplares.
good_reads_birthplace <- good_reads %>% group_by(birthplace)
good_reads_birthplace_count <- good_reads_birthplace %>% summarise(copies = n())

mostCommonBirthplaces <- head(good_reads_birthplace_count[order(good_reads_birthplace_count$copies, decreasing=TRUE),1:2],5)

good_reads_mostCommonBirthplaces <- filter(good_reads_birthplace, 
                                      any(mostCommonBirthplaces$birthplace == birthplace))

# Representamos las gráficas
p0 <- ggplot(good_reads_mostCommonGenres, aes(genres, pages)) + 
  labs(x = 'Género', y = 'Páginas')
p0 + coord_flip() + geom_boxplot()
p0 + coord_flip() + geom_point()

# Filtremos aquellos con menos páginas para verlo con mayor claridad
p1 <- ggplot(good_reads_mostCommonGenres[good_reads_mostCommonGenres$pages < 1500,], aes(genres, pages)) + 
  labs(x = 'Género', y = 'Páginas')
p1 + coord_flip() + geom_boxplot()

# Otras representaciones
p2 <- ggplot(good_reads_mostCommonGenres[good_reads_mostCommonGenres$num_ratings < 5e+5,], aes(genres, num_ratings)) + 
  labs(x = 'Género', y = 'Valoraciones')
p2 + coord_flip() + geom_boxplot()

p31 <- ggplot(good_reads_mostCommonGenres, aes(genres, book_average_rating)) + 
  labs(x = 'Género', y = 'Puntuación media')
p31 + coord_flip() + geom_boxplot()

p32 <- ggplot(good_reads_mostCommonBirthplaces, aes(birthplace, book_average_rating)) + 
  labs(x = 'Lugar de nacimiento', y = 'Puntuación media')
p32 + coord_flip() + geom_boxplot()

p41 <- ggplot(good_reads_mostCommonGenres[good_reads_mostCommonGenres$publish_year > 1850,], aes(genres, publish_year)) + 
  labs(x = 'Género', y = 'Fecha de publicación')
p41 + coord_flip() + geom_boxplot()

p42 <- ggplot(good_reads_mostCommonBirthplaces[good_reads_mostCommonBirthplaces$publish_year > 1850,], aes(birthplace, publish_year)) + 
  labs(x = 'Lugar de nacimiento', y = 'Fecha de publicación')
p42 + coord_flip() + geom_boxplot()

# Gráficos de barras
good_reads$rating_range <- ifelse(good_reads$author_average_rating < as.numeric(quantile(good_reads$author_average_rating,0.333)), 
                                  'Bajo',
                                  ifelse(good_reads$author_average_rating < as.numeric(quantile(good_reads$author_average_rating,0.666)), 
                                         'Medio',
                                         'Alto'))

good_reads$rating_range <- factor(good_reads$rating_range, levels=c('Bajo','Medio','Alto'))

ggplot(good_reads, aes(x = author_gender, fill = author_gender)) +
  geom_bar() +
  scale_fill_manual(values = c("pink1","steelblue1")) +
  facet_wrap(~rating_range) +
  labs(title = 'Distribuciones de valoración (0-5) de autores 
agrupadas por "hombre" y "mujer" en tres intervalos')

good_reads$rating_extreme_range <- ifelse(good_reads$author_average_rating < as.numeric(quantile(good_reads$author_average_rating,0.01)), 
                                  'Muy bajo',
                                  ifelse(good_reads$author_average_rating < as.numeric(quantile(good_reads$author_average_rating,0.99)), 
                                         'Medio',
                                         'Muy alto'))

good_reads$rating_extreme_range <- factor(good_reads$rating_extreme_range, levels=c('Muy bajo','Medio','Muy alto'))

ggplot(good_reads[good_reads$rating_extreme_range != 'Medio',], aes(x = author_gender, fill = author_gender)) +
  geom_bar() +
  scale_fill_manual(values = c("pink1","steelblue1")) +
  facet_wrap(~rating_extreme_range) +
  labs(title = 'Distribuciones de valoración (0-5) de autores 
agrupadas por "hombre" y "mujer" en intervalos extremos')
