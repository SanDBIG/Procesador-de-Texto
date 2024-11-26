# Carga de Paquetes -------------------------------------------------------

install.packages("pacman")
library(pacman)

p_load(readtext,
       tidytext,
       quanteda,
       stringr,
       ggplot2,
       wordcloud,
       quanteda.textplots)

# Uso de directorios Relativos --------------------------------------------

data_dir <- "data/entrevistas"       # Carpeta con los TXT
stopwords_dir <- "data/stopwords"    # Carpeta con las stopwords
output_dir <- "data/processed"       # Carpeta para guardar resultados

# Crear las carpetas cuando no existen ------------------------------------

if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
if (!dir.exists(stopwords_dir)) dir.create(stopwords_dir, recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Importar archivo de stopwords -------------------------------------------

# Asegúrate de que el archivo stopwords-es.txt esté en la carpeta `stopwords_dir`
stopwords_file <- file.path(stopwords_dir, "stopwords-es.txt")
if (!file.exists(stopwords_file)) {
  stop("El archivo de stopwords no se encuentra en la carpeta data/stopwords")
}

# Leer las stopwords desde el archivo
custom_stopwords <- readLines(stopwords_file, encoding = "UTF-8") # Leer el archivo línea por línea
all_stopwords <- c(stopwords("es"), custom_stopwords)  # Combinar con stopwords en español

# Importar archivos txt ---------------------------------------------------

text_data <- readtext(paste0(data_dir, "/*.txt")) # Leer todos los archivos TXT
print(text_data)  # Verificar contenido


# Limpiar texto y crear tokens --------------------------------------------

corpus <- corpus(text_data)

tokens <- tokens(corpus,
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(all_stopwords)  # Aplicar las stopwords combinadas

# Guardar tokens procesados -----------------------------------------------

saveRDS(tokens, file = file.path(output_dir, "tokens_processed.rds"))


# Crear nube de palabras y guardarla --------------------------------------

# Crear una matriz de frecuencias (DFM) a partir de los tokens
dfm <- dfm(tokens)

# Nube de palabras usando el DFM
png(file.path(output_dir, "wordcloud.png"), width = 800, height = 600)
textplot_wordcloud(dfm, max_words = 100)  # Usar el DFM en lugar de tokens
dev.off()  # Cerrar el dispositivo gráfico correctamente

