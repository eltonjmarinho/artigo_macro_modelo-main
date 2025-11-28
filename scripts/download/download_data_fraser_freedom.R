# Instala pacotes, se necessário
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")

# Carrega os pacotes
library(readxl)
library(dplyr)

# URL e nomes de arquivo
url <- "https://efotw.org/sites/all/modules/custom/ftw_maps_pages/files/efotw-2025-master-index-data-for-researchers-iso.xlsx"
dest_file <- "data/freedom.xlsx"
csv_file <- "data/freedom.csv"

# Cria o diretório se necessário
if (!dir.exists("data")) {
  dir.create("data")
}

# Baixa o arquivo
download.file(url, dest_file, mode = "wb")

# Lê o arquivo inteiro como dados brutos
raw_data <- read_excel(dest_file, col_names = FALSE)

# Encontra a linha do cabeçalho que contém "Year"
header_row_index <- which(apply(raw_data, 1, function(row) "Year" %in% row))[1]
if (is.na(header_row_index)) {
  stop("A coluna 'Year' não foi encontrada no arquivo Excel.")
}

# Extrai a linha do cabeçalho
header_row <- as.character(raw_data[header_row_index, ])

# **NOVO PASSO: Limpa e garante nomes de colunas únicos**
header_row[is.na(header_row)] <- "NA_col" # Substitui NAs por um nome
unique_headers <- make.unique(header_row, sep = ".") # Garante nomes únicos

# Extrai os dados e aplica os nomes de colunas únicos
data_body <- raw_data[(header_row_index + 1):nrow(raw_data), ]
colnames(data_body) <- unique_headers

# Converte a coluna 'Year' para tipo numérico
data_body$Year <- as.numeric(data_body$Year)

# Remove linhas onde a conversão de 'Year' falhou
data_body <- data_body %>% filter(!is.na(Year))

# Aplica o filtro para os anos 2010-2023
freedom_data_filtered <- data_body %>% 
  filter(Year >= 2010 & Year <= 2023)

# Salva o resultado final
write.csv(freedom_data_filtered, csv_file, row.names = FALSE)

# Remove o arquivo .xlsx original
file.remove(dest_file)

cat(paste("Processo finalizado com sucesso. Arquivo salvo em", csv_file, "\n"))