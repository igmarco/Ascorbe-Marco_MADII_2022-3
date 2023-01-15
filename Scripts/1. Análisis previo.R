# Paquetes
if(!require(readr)){
  install.packages("readr", dependencies = TRUE, repos='http://cran.rstudio.com/')
  require(readr)
}
if(!require(psych)){
  install.packages("psych")
  require(psych)
}

# if(!require(ggplot2)){
#   install.packages("ggplot2")
#   require(ggplot2)
# }
# if(!require(GGally)){
#   install.packages("GGally")
#   require(GGally)
# }
# if(!require(car)){
#   install.packages("car")
#   require(car)
# }
# if(!require(patchwork)){
#   install.packages("patchwork")
#   require(patchwork)
# }

# Importación
ruta_csv <- "Datos/Brutos/good_reads_final.csv"
good_reads <- as.data.frame(read_csv(ruta_csv, col_names = TRUE))

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

# Comparación de los subconjuntos
colMeans(good_reads_female[c("author_average_rating",
                              "author_rating_count", 
                              "author_review_count", 
                              "book_average_rating", 
                              "num_ratings", 
                              "num_reviews", 
                              "pages", 
                              "publish_year", 
                              "score"),])
colMeans(good_reads_male)




