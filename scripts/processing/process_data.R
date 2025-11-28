# Carrega pacotes necessários
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)


# Define o caminho para a pasta de dados
data_path <- "data"
processed_data_path <- file.path(data_path, "processed")

# Cria a pasta 'processed' se não existir
if (!dir.exists(processed_data_path)) {
  dir.create(processed_data_path, recursive = TRUE)
  message("Pasta 'data/processed' criada.")
} else {
  message("Pasta 'data/processed' já existe.")
}

# Caminhos dos arquivos de entrada
freedom_file <- file.path(data_path, "freedom.csv")
wdi_file <- file.path(data_path, "wdi_data.csv")

# Carrega os dados brutos
# read.csv por padrão converte nomes de colunas inválidos, 
# substituindo espaços por pontos. Ex: "ECONOMIC FREEDOM ALL AREAS" -> "ECONOMIC.FREEDOM.ALL.AREAS"
df_freedom_raw <- read.csv(freedom_file)
df_wdi_raw <- read.csv(wdi_file)

# --- Processamento do df_freedom ---
# 1. Seleciona e renomeia as colunas de interesse
# O read.csv converte o nome "ECONOMIC FREEDOM ALL AREAS" para "ECONOMIC.FREEDOM.ALL.AREAS"
normalize_label <- function(x) {
  gsub("[^[:alnum:]]", "", tolower(x))
}

area_targets <- c(
  area1 = "Area 1 Size of Government",
  area2 = "Area 2 Legal System & Property Rights -- With Gender Adjustment",
  area3 = "Area 3 Sound Money",
  area4 = "Area 4 Freedom to trade internationally",
  area5 = "Area 5 Regulation"
)

available_cols <- setNames(names(df_freedom_raw), normalize_label(names(df_freedom_raw)))

area_lookup <- vapply(area_targets, function(label) {
  key <- normalize_label(label)
  if (!key %in% names(available_cols)) {
    stop(sprintf("Coluna '%s' não encontrada no freedom.csv", label))
  }
  available_cols[[key]]
}, FUN.VALUE = character(1))

df_freedom <- df_freedom_raw %>%
  mutate(
    !!!setNames(
      lapply(names(area_lookup), function(nm) df_freedom_raw[[area_lookup[[nm]]]]),
      names(area_lookup)
    )
  ) %>%
  select(
    Year,
    iso3c = ISO_Code,
    Countries,
    `World Bank Region` = World.Bank.Region,
    `World Bank Current Income Classification, 1990-Present` = World.Bank.Current.Income.Classification..1990.Present,
    `ECONOMIC FREEDOM ALL AREAS` = ECONOMIC.FREEDOM.ALL.AREAS,
    area1,
    area2,
    area3,
    area4,
    area5
  )

# --- Processamento do df_wdi ---
# 1. Remove a coluna 'status', se existir
if("status" %in% colnames(df_wdi_raw)){
  df_wdi_raw <- df_wdi_raw %>% select(-status)
}
# 2. Seleciona e renomeia as colunas de interesse
df_wdi <- df_wdi_raw %>%
  select(
    year,
    iso3c,
    country, # Corresponds to user's 'countryname'
    region, # Corresponds to user's 'wb_region'
    income, # Corresponds to user's 'wb_income_group'
    `NY.GDP.PCAP.KD`,    # Per capita GDP
    `NE.GDI.FTOT.KD.ZG`,  # Gross fixed capital formation
    `SE.SEC.ENRR`,       # Secondary school enrolment
    `FP.CPI.TOTL.ZG`,    # The inflation rate
    `FS.AST.PRVT.GD.ZS`, # Credit to the private sector
    `BX.TRF.PWKR.DT.GD.ZS`, # Foreign remittance from citizens abroad
    `FR.INR.RINR`,       # The real interest rate
    `SL.TLF.CACT.ZS`     # The labor force participation rate
  )

# Remove linhas com valores NA dos dataframes com colunas já selecionadas
df_freedom <- na.omit(df_freedom)
df_wdi <- na.omit(df_wdi)

message("Arquivos carregados e processados com sucesso:")
message(paste("  - freedom_processed.csv:", nrow(df_freedom), "linhas,", ncol(df_freedom), "colunas"))
message(paste("  - wdi_processed.csv:", nrow(df_wdi), "linhas,", ncol(df_wdi), "colunas"))

# --- A partir daqui, você pode adicionar o código para tratamento e unificação dos dados ---
# Exemplo: Visualizar as primeiras linhas de cada dataframe
# print("Primeiras linhas de df_freedom:")
# head(df_freedom)
# print("Primeiras linhas de df_wdi:")
# head(df_wdi)

# Salva os dataframes processados na pasta 'processed'
write.csv(df_freedom, file.path(processed_data_path, "freedom_processed.csv"), row.names = FALSE)
write.csv(df_wdi, file.path(processed_data_path, "wdi_processed.csv"), row.names = FALSE)

message("Dataframes salvos na pasta 'data/processed'.")

# Limpa a memória
rm(list = ls())
message("Memória limpa.")
