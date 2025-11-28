# Carrega pacotes necessários
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr") # Para pivot_longer
library(dplyr)
library(knitr)
library(tidyr)
if (!requireNamespace("car", quietly = TRUE)) install.packages("car") # For VIF
library(car)

# Define o caminho do arquivo de dados do painel balanceado
file_path <- file.path("data", "processed", "data_model.csv")

# Verifica se o arquivo existe antes de tentar ler
if (!file.exists(file_path)) {
  stop(paste("Erro: O arquivo", file_path, "não foi encontrado. Por favor, execute o script 'scripts/processing/panel_balancing.R' primeiro para gerar o arquivo."))
}

# Carrega o dataframe
df_balanced <- read.csv(file_path)
message(paste("Arquivo", basename(file_path), "carregado com sucesso."))

# --- Diagnóstico para lab_part ---
cat("\n--- Diagnóstico da variável 'lab_part' ---\n")
if ("lab_part" %in% colnames(df_balanced)) {
  cat("Variável 'lab_part' encontrada no dataframe.\n")
  cat("Resumo de 'lab_part':\n")
  print(summary(df_balanced$lab_part))
  cat(paste("Número de valores NA em 'lab_part':", sum(is.na(df_balanced$lab_part)), "\n"))
} else {
  cat("Variável 'lab_part' NÃO encontrada no dataframe.\n")
}
cat("------------------------------------------\n")

# Conta o número de anos (observações) por iso3c
years_per_country <- df_balanced %>%
  group_by(iso3c) %>%
  summarise(count_years = n(), .groups = 'drop')


# Exibe a contagem no terminal
cat("\n--- Contagem de Anos por País (iso3c) no Painel Balanceado ---\n\n")
# Usando print() para formatar a saída do tibble/dataframe de forma legível
print(years_per_country, n = Inf) # n = Inf para garantir que todas as linhas sejam impressas

# Exibe um resumo para confirmar que o painel está balanceado
if (nrow(years_per_country) > 0) {
  unique_counts <- unique(years_per_country$count_years)
  cat("\n--- Resumo da Verificação ---\n")
  cat(paste("Número total de países no painel:", nrow(years_per_country), "\n"))
  if (length(unique_counts) == 1) {
    cat(paste("Verificação de balanceamento: SUCESSO. Todos os países têm", unique_counts, "anos de dados.\n"))
  } else {
    cat("Verificação de balanceamento: FALHA. O número de anos varia entre os países.\n")
    print(summary(years_per_country$count_years))
  }
} else {
  cat("\nO dataframe está vazio. Nenhuma análise pôde ser feita.\n")
}

# --- Análise por Região e Renda do Banco Mundial ---

# Tabela 1: Conta o número de países únicos por região
countries_by_region <- df_balanced %>%
  group_by(wb_region) %>%
  summarise(num_countries = n_distinct(iso3c), .groups = 'drop') %>%
  arrange(wb_region)

cat("\n--- Contagem de Países por Região ---\n\n")
print(countries_by_region, n = Inf)

# Tabela 2: Conta o número de países únicos por grupo de renda
countries_by_income <- df_balanced %>%
  group_by(wb_income_group) %>%
  summarise(num_countries = n_distinct(iso3c), .groups = 'drop') %>%
  arrange(wb_income_group)

cat("\n--- Contagem de Países por Grupo de Renda ---\n\n")
print(countries_by_income, n = Inf)

# Tabela 3: Conta o número total de observações por grupo de renda
observations_by_income <- df_balanced %>%
  group_by(wb_income_group) %>%
  summarise(total_observations = n(), .groups = 'drop') %>%
  arrange(wb_income_group)

cat("\n--- Contagem de Observações por Grupo de Renda ---\n\n")
print(observations_by_income, n = Inf)

# --- Tabela de Estatísticas Descritivas por Grupo de Renda ---

# Define as variáveis numéricas para sumarizar
numeric_vars <- c("gdp", "inv", "edu", "inf", "credit", "rem", "intr", "lab_part", "ef")

# Garante que as variáveis são numéricas e remove NAs para a sumarização
df_data_clean <- df_balanced %>% # Use df_balanced as it's the main dataframe
  select(wb_income_group, all_of(numeric_vars)) %>%
  mutate(across(all_of(numeric_vars), as.numeric)) %>%
  drop_na() # Remove linhas com NA para garantir estatísticas precisas

# Calcula as estatísticas descritivas por grupo de renda
summary_stats <- df_data_clean %>%
  group_by(wb_income_group) %>%
  summarise(
    across(all_of(numeric_vars),
           list(
             N = ~n(),
             Mean = ~mean(., na.rm = TRUE),
             Median = ~median(., na.rm = TRUE), # Added Median
             SD = ~sd(., na.rm = TRUE),
             CV = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE), # Added CV
             Min = ~min(., na.rm = TRUE),
             Max = ~max(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()

# Reorganiza a tabela para um formato mais apresentável (variáveis como linhas)
summary_table_long <- summary_stats %>%
  pivot_longer(
    cols = -wb_income_group,
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(N|Mean|Median|SD|CV|Min|Max)" # Added Median and CV
  )

# Formata a tabela para exibição acadêmica
# Para cada grupo de renda, imprimir uma tabela separada
cat("\n--- Tabela de Estatísticas Descritivas por Grupo de Renda ---\n\n")

income_groups <- unique(summary_table_long$wb_income_group)

for (group in income_groups) {
  cat(paste0("### Grupo de Renda: ", group, "\n\n"))
  
  table_for_group <- summary_table_long %>%
    filter(wb_income_group == group) %>%
    select(Variable, N, Mean, Median, SD, CV, Min, Max) %>% # Explicitly select columns
    mutate(
      Mean = sprintf("%.2f", Mean),
      Median = sprintf("%.2f", Median), # Added Median
      SD = sprintf("%.2f", SD),
      CV = sprintf("%.2f", CV), # Added CV
      Min = sprintf("%.2f", Min),
      Max = sprintf("%.2f", Max)
    )
  
  # Renomeia as colunas para melhor apresentação
  colnames(table_for_group) <- c("Variável", "N", "Média", "Mediana", "Desvio Padrão", "Coeficiente de Variação", "Mínimo", "Máximo") # Added Mediana and Coeficiente de Variação
  
  print(kable(table_for_group, format = "markdown", align = "lcccccc")) # Updated align
  cat("\n\n")
}

message("\nEstatísticas descritivas geradas com sucesso.")

# --- Análise de Correlação e VIF para o Dataset Completo ---
cat("\n--- Análise de Correlação e VIF para o Dataset Completo ---\n\n")

# Seleciona apenas as variáveis independentes para o dataset completo
# Exclui 'inv' (variável dependente)
independent_vars <- numeric_vars[numeric_vars != "inv"]
df_full_independent <- df_data_clean %>%
  select(all_of(independent_vars))

# Remove linhas com NA para a análise de correlação e VIF
df_full_independent <- na.omit(df_full_independent)

if (nrow(df_full_independent) > 0 && ncol(df_full_independent) > 1) {
  # Calcula e imprime a matriz de correlação
  correlation_matrix <- cor(df_full_independent)
  cat("#### Matriz de Correlação:\n")
  print(kable(correlation_matrix, format = "markdown", digits = 2))
  cat("\n")

  # Calcula e imprime os valores de VIF
  if (ncol(df_full_independent) >= 2) {
    vif_model_formula_str <- paste(colnames(df_full_independent)[1], "~", paste(colnames(df_full_independent)[-1], collapse = " + "))
    vif_model <- lm(as.formula(vif_model_formula_str), data = df_full_independent)
    
    vif_results <- vif(vif_model)
    vif_values <- data.frame(Variable = names(vif_results), VIF = as.numeric(vif_results))
    cat("#### Valores de VIF:\n")
    print(kable(vif_values, format = "markdown", digits = 2))
    cat("\n")
  } else {
    cat("Não há variáveis independentes suficientes para calcular o VIF (mínimo 2).\n\n")
  }
} else {
  cat("Não há dados suficientes ou variáveis independentes para análise de correlação e VIF no dataset completo.\n\n")
}
cat("\n") # Add a newline for spacing


# --- Análise do Número de Países por Ano ---


# Calcula o número de países com dados completos por ano
countries_per_year <- df_balanced %>%
  group_by(year) %>%
  summarise(num_countries = n_distinct(iso3c), .groups = 'drop')

# Exibe a contagem no terminal
cat("\n--- Contagem de Países por Ano ---\n\n")
print(countries_per_year, n = Inf)

# Carrega ggplot2 para visualização
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Cria o gráfico de barras
plot_countries_per_year <- ggplot(countries_per_year, aes(x = as.factor(year), y = num_countries)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Número de Países com Dados por Ano",
    x = "Ano",
    y = "Número de Países"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Define o caminho para salvar o gráfico
output_dir <- file.path("views", "www")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
plot_output_path <- file.path(output_dir, "countries_per_year_distribution.png")

# Salva o gráfico
ggsave(plot_output_path, plot = plot_countries_per_year, width = 12, height = 7, dpi = 300)

cat(paste("\nGráfico da distribuição de países por ano salvo em:", plot_output_path, "\n"))


# --- Tabela de Países por Grupo de Renda ---
cat("\n--- Tabela de Países por Grupo de Renda ---\n\n")

# Agrupa os nomes dos países por grupo de renda, garantindo que cada país seja listado apenas uma vez por grupo
countries_by_income_group_table <- df_balanced %>%
  select(wb_income_group, countryname) %>%
  distinct() %>%
  group_by(wb_income_group) %>%
  summarise(Paises = paste(sort(countryname), collapse = ", "), .groups = 'drop') %>%
  arrange(wb_income_group)

# Itera sobre a tabela e imprime cada grupo para evitar o truncamento pelo console
for (i in 1:nrow(countries_by_income_group_table)) {
  cat(paste("Grupo de Renda:", countries_by_income_group_table$wb_income_group[i], "\n"))
  # A função strwrap vai quebrar a linha de países de forma inteligente
  wrapped_paises <- strwrap(paste("Países:", countries_by_income_group_table$Paises[i]), width = 100, exdent = 2)
  cat(paste(wrapped_paises, collapse = "\n"), "\n\n")
}

cat("\n\n")


# Limpa a memória
rm(list = ls())
message("\nMemória limpa.")