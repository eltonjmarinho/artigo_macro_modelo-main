# Instala o pacote WDI, se necessário
#if (!require("WDI")) {
#  install.packages("WDI")
#}

# Carrega o pacote WDI
library(WDI)

# Define os indicadores desejados
indicators <- c(
  "NY.GDP.PCAP.KD",    # Per capita GDP
  "NE.GDI.FTOT.KD.ZG",  # Gross fixed capital formation
  "SE.SEC.ENRR",       # Secondary school enrolment
  "FP.CPI.TOTL.ZG",    # The inflation rate
  "FS.AST.PRVT.GD.ZS", # Credit to the private sector
  "BX.TRF.PWKR.DT.GD.ZS", # Foreign remittance from citizens abroad
  "FR.INR.RINR",       # The real interest rate
  "SL.TLF.CACT.ZS"     # The labor force participation rate
)

# Baixa os dados para todos os países de 2000 a 2023
wdi_data <- WDI(
  country = "all",
  indicator = indicators,
  start = 2010,
  end = 2023,
  extra = TRUE,
  cache = NULL
)

# Cria o diretório de dados se ele não existir
if (!dir.exists("data")) {
  dir.create("data")
}

# Salva os dados em um arquivo CSV
write.csv(wdi_data, file = "data/wdi_data.csv", row.names = FALSE)

cat("Download dos dados do WDI concluído. Arquivo salvo em data/wdi_data.csv\n")