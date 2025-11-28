
# Carrega pacotes necessários
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(tidyr)

# Define o caminho para a pasta de dados processados
processed_data_path <- file.path("data", "processed")

# Caminhos dos arquivos de entrada processados
freedom_processed_file <- file.path(processed_data_path, "freedom_processed.csv")
wdi_processed_file <- file.path(processed_data_path, "wdi_processed.csv")

# Carrega os dataframes processados
df_freedom_proc <- read.csv(freedom_processed_file) %>%
  rename(year = Year, countryname = Countries) %>% # Renomeia colunas para padronização
  distinct(iso3c, year, .keep_all = TRUE) # Remove duplicatas para iso3c e year
df_wdi_proc <- read.csv(wdi_processed_file) %>%
  rename(countryname = country) %>% # Renomeia colunas para padronização
  distinct(iso3c, year, .keep_all = TRUE) # Remove duplicatas para iso3c e year

message("Arquivos processados carregados com sucesso:")
message(paste("  - freedom_processed.csv:", nrow(df_freedom_proc), "linhas,", ncol(df_freedom_proc), "colunas"))
message(paste("  - wdi_processed.csv:", nrow(df_wdi_proc), "linhas,", ncol(df_wdi_proc), "colunas"))

# --- Início do merge e seleção de colunas ---

# Assumindo que o merge será feito por 'iso3c' e 'year'
# É crucial que todos os dataframes tenham essas colunas e que 'year' seja numérica.

# Primeiro merge: df_freedom_proc e df_wdi_proc
df_final <- df_freedom_proc %>%
  full_join(df_wdi_proc, by = c("iso3c", "year")) %>%
  mutate(countryname = coalesce(countryname.x, countryname.y))

# Seleciona e renomeia as colunas para o modelo final
df_model <- df_final %>% 
  select(
    year,
    iso3c,
    countryname,
    wb_region = `World.Bank.Region`,
    wb_income_group = `World.Bank.Current.Income.Classification..1990.Present`,
    gdp = NY.GDP.PCAP.KD,
    inv = NE.GDI.FTOT.KD.ZG,
    edu = SE.SEC.ENRR,
    inf = FP.CPI.TOTL.ZG,
    credit = FS.AST.PRVT.GD.ZS,
    rem = BX.TRF.PWKR.DT.GD.ZS,
    intr = FR.INR.RINR,
    lab_part = SL.TLF.CACT.ZS,
    ef = ECONOMIC.FREEDOM.ALL.AREAS,
    area1,
    area2,
    area3,
    area4,
    area5
  ) %>%
  drop_na() # Remove linhas com NA em qualquer uma das colunas selecionadas

# Salva a tabela final
output_file <- file.path(processed_data_path, "data_model.csv")
write.csv(df_model, output_file, row.names = FALSE)

message(paste0("Tabela unificada salva como ", output_file))

# Limpa a memória
rm(list = ls())
message("Memória limpa.")
