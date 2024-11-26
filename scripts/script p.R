# Carga de Paquetes -------------------------------------------------------

install.packages("pacman")
library(pacman)

p_load(readtext,
       tidytext,
       quanteda,
       stringr,
       ggplot2,
       wordcloud,
       quanteda.textplots,
       topicmodels,
       igraph)

# Uso de directorios Relativos --------------------------------------------

data_dir <- "data/entrevistas"       # Carpeta con los TXT
stopwords_dir <- "data/stopwords"    # Carpeta con las stopwords
output_dir <- "data/processed"       # Carpeta para guardar resultados

# Crear las carpetas cuando no existen ------------------------------------

if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
if (!dir.exists(stopwords_dir)) dir.create(stopwords_dir, recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Importar archivo de stopwords -------------------------------------------

stopwords_file <- file.path(stopwords_dir, "stopwords-es.txt")
if (!file.exists(stopwords_file)) {
  stop("El archivo de stopwords no se encuentra en la carpeta data/stopwords")
}

custom_stopwords <- readLines(stopwords_file, encoding = "UTF-8")
all_stopwords <- c(stopwords("es"), custom_stopwords)

# Importar archivos txt ---------------------------------------------------

text_data <- readtext(paste0(data_dir, "/*.txt")) 
print(text_data)

# Limpiar texto y crear tokens --------------------------------------------

corpus <- corpus(text_data)

tokens <- tokens(corpus,
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(all_stopwords)

# Guardar tokens procesados -----------------------------------------------

saveRDS(tokens, file = file.path(output_dir, "tokens_processed.rds"))

# Crear una matriz de frecuencias (DFM) -----------------------------------

dfm <- dfm(tokens)

# Crear nube de palabras --------------------------------------------------

png(file.path(output_dir, "wordcloud.png"), width = 800, height = 600)
textplot_wordcloud(dfm, max_words = 100)
dev.off()

# Codificación Manual -----------------------------------------------------

library(stringr)
text_data$code <- ifelse(str_detect(text_data$text, "sostenibilidad"), "Sostenibilidad", "Sin Código")
write.csv(text_data, file = file.path(output_dir, "text_with_codes.csv"), row.names = FALSE)

# Análisis de Frecuencia --------------------------------------------------

top_terms <- topfeatures(dfm, n = 10)
print(top_terms)

# Guardar términos frecuentes en un archivo
write.csv(as.data.frame(top_terms), file = file.path(output_dir, "top_terms.csv"), row.names = TRUE)

# Gráfico de términos más frecuentes
freq_df <- data.frame(term = names(top_terms), freq = top_terms)
png(file.path(output_dir, "term_frequency.png"), width = 800, height = 600)
ggplot(freq_df, aes(x = reorder(term, freq), y = freq)) +
  geom_col() +
  coord_flip() +
  labs(title = "Términos más frecuentes", x = "Término", y = "Frecuencia")
dev.off()

# Modelado de Temas (Topic Modeling) --------------------------------------

lda_model <- LDA(convert(dfm, to = "topicmodels"), k = 3) # 3 temas
lda_terms <- terms(lda_model, 5) # Las 5 palabras más representativas por tema
write.csv(as.data.frame(lda_terms), file = file.path(output_dir, "lda_topics.csv"))
print(lda_terms)

# Análisis de Co-ocurrencia -----------------------------------------------

fcm <- fcm(tokens)
png(file.path(output_dir, "cooccurrence_network.png"), width = 800, height = 600)
textplot_network(fcm, min_freq = 0.1)
dev.off()

# Análisis de Sentimiento -------------------------------------------------

sentiment <- get_sentiments("bing") # Cambiar por "nrc" para emociones
sentiment_data <- tokens %>%
  tokens_lookup(dictionary = sentiment) %>%
  dfm() %>%
  convert(to = "data.frame")

write.csv(sentiment_data, file = file.path(output_dir, "sentiment_analysis.csv"))
print(head(sentiment_data))

