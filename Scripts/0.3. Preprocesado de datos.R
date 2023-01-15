# Paquetes
if(!require(readr)){
  install.packages("readr", dependencies = TRUE, repos='http://cran.rstudio.com/')
  require(readr)
}
if(!require(psych)){
  install.packages("psych")
  require(psych)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}

# Importación
ruta_csv <- "Datos/Brutos/good_reads_final.csv"
good_reads <- as.data.frame(read_csv(ruta_csv, col_names = TRUE))

# Resúmenes
colnames(good_reads)
head(good_reads, 5)

summary(good_reads)
describe(good_reads)

# Filtrado de las columnas relevantes
good_reads <- select(good_reads,-'author_id',-'author_name',-'author_page_url',
                     -'book_fullurl',-'book_id',-'book_title')

colnames(good_reads)

# División de las columnas
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
                         "publish_date",
                         "score")

# Estudio de las variables categóricas
unique(good_reads$author_gender)
unique(good_reads$author_genres)
unique(good_reads$birthplace)
unique(good_reads$genre_1)
unique(good_reads$genre_2)

# Preprocesado de la variable "author_genres"
good_reads$author_genres <- strsplit(as.character(good_reads$author_genres), ",")
unique(good_reads$author_genres)
good_reads[970,"author_genres"]

# Preprocesado de la variable "birthplace"
good_reads$birthplace <- gsub("\n   ", "", good_reads$birthplace)
good_reads$birthplace <- gsub("\n  ", "", good_reads$birthplace)
good_reads$birthplace <- gsub("[\n\r]", "", good_reads$birthplace)
unique(good_reads$birthplace)

# Sustituimos "" y valores anómalos por nulos.
good_reads$birthplace[good_reads$birthplace == ""] <- NA
good_reads$birthplace[good_reads$birthplace == "Occupied"] <- NA
good_reads$birthplace[good_reads$birthplace == "Republic of"] <- NA
good_reads$birthplace[good_reads$birthplace == "Identity of author to remamystery for now"] <- NA
good_reads$birthplace[good_reads$birthplace == "I've continued \"worlding\" to many countries."] <- NA

# Últimas correcciones
good_reads$birthplace[good_reads$birthplace == "Seoul"] <- "South Korea"

good_reads$birthplace[good_reads$birthplace == "British India" |
                        good_reads$birthplace == "ndia"] <- "India"

good_reads$birthplace[good_reads$birthplace == "Hong Kong"] <- "China"

good_reads$birthplace[good_reads$birthplace == "Cape Town"] <- "South Africa"

good_reads$birthplace[good_reads$birthplace == "Melbourne"] <- "Australia"

good_reads$birthplace[good_reads$birthplace == "Jerusalem"] <- "Israel"

good_reads$birthplace[good_reads$birthplace == "the Former Yugoslav Republic of"] <- "Yugoslav Republic"

good_reads$birthplace[good_reads$birthplace == "Holy See (vatican City State)"] <- "Vatican City State"

good_reads$birthplace[good_reads$birthplace == "Winnipeg" | 
                      good_reads$birthplace == "currently living Canada" | 
                        good_reads$birthplace == "MB  Canada" | 
                        good_reads$birthplace == "Vancouver" | 
                        good_reads$birthplace == "Canada "] <- "Canada"

good_reads$birthplace[good_reads$birthplace == "Birmingham" |
                        good_reads$birthplace == "London" |
                        good_reads$birthplace == "British Indian Ocean Territory" |
                        good_reads$birthplace == "Gibraltar" |
                        good_reads$birthplace == "Buckinghamshire" |
                        good_reads$birthplace == "North Wales" |
                        good_reads$birthplace == "Westminister CA" |
                        good_reads$birthplace == "Northern Ireland" |
                        good_reads$birthplace == "British" |
                        good_reads$birthplace == "Lancashire" |
                        good_reads$birthplace == "Scotland" |
                        good_reads$birthplace == "Wales"] <- "United Kingdom"

good_reads$birthplace[good_reads$birthplace == "Colorado" | 
                        good_reads$birthplace == "Chicago" | 
                        good_reads$birthplace == "Alabama" | 
                        good_reads$birthplace == "Southern California" | 
                        good_reads$birthplace == "New York" | 
                        good_reads$birthplace == "New Jersey" | 
                        good_reads$birthplace == "Missouri" | 
                        good_reads$birthplace == "Hawaii" | 
                        good_reads$birthplace == "Boston" | 
                        good_reads$birthplace == "Pennsylvania" | 
                        good_reads$birthplace == "Massachusetts" | 
                        good_reads$birthplace == "Kentucky" | 
                        good_reads$birthplace == "Michigan" | 
                        good_reads$birthplace == "Minnesota" | 
                        good_reads$birthplace == "Springfield" | 
                        good_reads$birthplace == "Minneapolis" | 
                        good_reads$birthplace == "Salt Lake City" | 
                        good_reads$birthplace == "Maryland" | 
                        good_reads$birthplace == "Jordan" | 
                        good_reads$birthplace == "New York City" | 
                        good_reads$birthplace == "Baltimore" | 
                        good_reads$birthplace == "Illinois" | 
                        good_reads$birthplace == "Ohio" | 
                        good_reads$birthplace == "New York City " | 
                        good_reads$birthplace == "Rocky Mount" | 
                        good_reads$birthplace == "Philadelphia" | 
                        good_reads$birthplace == "Indianapolis" | 
                        good_reads$birthplace == "Cleveland" | 
                        good_reads$birthplace == "Minot" | 
                        good_reads$birthplace == "New York " | 
                        good_reads$birthplace == "Ohio " | 
                        good_reads$birthplace == "Dallas" | 
                        good_reads$birthplace == "Indiana" | 
                        good_reads$birthplace == "North Carolina" | 
                        good_reads$birthplace == "Los Angeles" | 
                        good_reads$birthplace == "San Francisco" | 
                        good_reads$birthplace == "New Orleans" | 
                        good_reads$birthplace == "Wichita Falls" | 
                        good_reads$birthplace == "South Dakota" | 
                        good_reads$birthplace == "Brooklyn" | 
                        good_reads$birthplace == "California" | 
                        good_reads$birthplace == "a phoenix egg" | 
                        good_reads$birthplace == "Rhode Island" | 
                        good_reads$birthplace == "Ft. Lauderdale" | 
                        good_reads$birthplace == "http://nikkiloftin.com/" | 
                        good_reads$birthplace == "Virginia" | 
                        good_reads$birthplace == "Florida" | 
                        good_reads$birthplace == "Alaska" | 
                        good_reads$birthplace == "Gary IndiANa" | 
                        good_reads$birthplace == "Seattle" | 
                        good_reads$birthplace == "Delaware" | 
                        good_reads$birthplace == "Manhattan" | 
                        good_reads$birthplace == "KY " | 
                        good_reads$birthplace == "D.C." | 
                        good_reads$birthplace == "N.Y." | 
                        good_reads$birthplace == "WI" | 
                        good_reads$birthplace == "IL" | 
                        good_reads$birthplace == "MA" | 
                        good_reads$birthplace == "CA" | 
                        good_reads$birthplace == "CT" | 
                        good_reads$birthplace == "Somewhere snowy MN" | 
                        good_reads$birthplace == "AL" | 
                        good_reads$birthplace == "DC" | 
                        good_reads$birthplace == "NYC" | 
                        good_reads$birthplace == "IL " | 
                        good_reads$birthplace == "MN" | 
                        good_reads$birthplace == "VA" | 
                        good_reads$birthplace == "NY" | 
                        good_reads$birthplace == "AZ" | 
                        good_reads$birthplace == "KS" | 
                        good_reads$birthplace == "MI" | 
                        good_reads$birthplace == "OH" | 
                        good_reads$birthplace == "KY" | 
                        good_reads$birthplace == "TX" | 
                        good_reads$birthplace == "NH" | 
                        good_reads$birthplace == "PA"] <- "United States"

good_reads$birthplace[grep("France", good_reads$birthplace)] <- "France"
good_reads$birthplace[grep("Russia", good_reads$birthplace)] <- "Russia"
good_reads$birthplace[grep("Pakistan", good_reads$birthplace)] <- "Pakistan"

unique(good_reads$birthplace)

# Estudio de las variables numéricas
describe(good_reads[variables_numericas])

unique(good_reads$author_average_rating)
unique(good_reads$author_rating_count)
unique(good_reads$author_review_count)
unique(good_reads$book_average_rating)
unique(good_reads$num_ratings)
unique(good_reads$num_reviews)
unique(good_reads$pages)
unique(good_reads$publish_date)
unique(good_reads$score)

# Preprocesado de la variable "publish_date"
good_reads$publish_year <- sapply(good_reads$publish_date, getYear)

unique(good_reads$publish_year)

# Eliminamos los valores atípicos.
substr(good_reads$publish_year,nchar(good_reads$publish_year)-1,nchar(good_reads$publish_year)+1)

good_reads$publish_year[good_reads$publish_year == "by" |
                          good_reads$publish_year == "1" |
                          good_reads$publish_year == "0" |
                          substr(good_reads$publish_year,nchar(good_reads$publish_year)-1,nchar(good_reads$publish_year)+1) == "st" |
                          substr(good_reads$publish_year,nchar(good_reads$publish_year)-1,nchar(good_reads$publish_year)+1) == "nd" |
                          substr(good_reads$publish_year,nchar(good_reads$publish_year)-1,nchar(good_reads$publish_year)+1) == "rd" |
                          substr(good_reads$publish_year,nchar(good_reads$publish_year)-1,nchar(good_reads$publish_year)+1) == "th"] <- NA

unique(good_reads$publish_year)

good_reads <- select(good_reads,-'publish_date')

# Guardamos los datos preprocesados.
# ruta_proc_csv <- "Datos/Procesados/good_reads_final_preprocesado.csv"
# write_csv(good_reads, ruta_proc_csv, col_names = TRUE) # Guardándolo como CSV convierte en NA la variable "author_genres"
ruta_proc_rda <- "Datos/Procesados/good_reads_final_preprocesado.rda"
save(good_reads, file = ruta_proc_rda)


# Importación
ruta_read_rda <- "Datos/Procesados/good_reads_final_preprocesado.rda"
load(file = ruta_read_rda)

head(good_reads)
