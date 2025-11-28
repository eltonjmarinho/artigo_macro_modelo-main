# Carrega pacotes necessários
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Define os caminhos dos arquivos
input_file <- file.path("data", "processed", "data_model.csv")
output_file <- file.path("data", "processed", "data_model_balanced.csv")

# Carrega o dataframe
df_model <- read.csv(input_file)
message("Dataframe 'data_model.csv' carregado.")

# Define o intervalo de anos desejado e o número de anos esperado
start_year <- 2011
end_year <- 2022
expected_years <- length(start_year:end_year)

# Filtra os países que têm dados para todos os anos no intervalo especificado
countries_with_all_years <- df_model %>%
  filter(year >= start_year & year <= end_year) %>%
  group_by(iso3c) %>%
  summarise(num_years = n_distinct(year), .groups = 'drop') %>%
  filter(num_years == expected_years) %>%
  pull(iso3c)

# Filtra o dataframe original para manter apenas os países e anos do painel balanceado
df_balanced_panel <- df_model %>%
  filter(iso3c %in% countries_with_all_years & year >= start_year & year <= end_year)

# Salva o novo dataframe balanceado
write.csv(df_balanced_panel, output_file, row.names = FALSE)

# Exibe um resumo do resultado
message("\n--- Resumo do Painel Balanceado ---")
message(paste("Intervalo de anos:", start_year, "-", end_year))
message(paste("Número de países com dados completos:", length(countries_with_all_years)))
message(paste("Número total de observações (países * anos):", nrow(df_balanced_panel)))
message(paste("Arquivo balanceado salvo em:", output_file))

# Limpa a memória
rm(list = ls())
message("\nMemória limpa.")
