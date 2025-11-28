# --- Script para analisar valores ausentes (NA) em arquivos CSV ---

# Função para contar NAs em cada coluna de um arquivo CSV
count_na_in_csv <- function(file_path) {
  # Verifica se o arquivo existe
  if (!file.exists(file_path)) {
    cat(paste("Arquivo não encontrado:", file_path, "\n"))
    return(NULL)
  }
  
  # Lê o arquivo CSV
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Calcula o número de NAs por coluna
  na_counts <- colSums(is.na(data))
  
  # Ordena as contagens em ordem decrescente
  sorted_na_counts <- sort(na_counts, decreasing = TRUE)
  
  # Imprime os resultados
  cat(paste("Contagem de valores NA para o arquivo:", file_path, "\n"))
  print(sorted_na_counts)
  cat("\n")
  
  return(sorted_na_counts)
}

# --- Análise dos arquivos ---

# Arquivos a serem analisados
wdi_file <- "data/wdi_data.csv"
freedom_file <- "data/freedom.csv"

# Executa a análise para cada arquivo
cat("--- Análise de Dados Ausentes ---\n\n")
count_na_in_csv(wdi_file)
count_na_in_csv(freedom_file)

cat("--- Fim da Análise ---\n")

