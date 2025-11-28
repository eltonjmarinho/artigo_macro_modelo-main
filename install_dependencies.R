# --- Script para Instalar Dependências do Projeto ---

# Este script verifica e instala todos os pacotes R necessários para a análise.

cat("Iniciando a instalação de dependências...\n\n")

# Função para instalar pacotes se não estiverem presentes
install_if_missing <- function(pkg_name, install_func) {
  if (!require(pkg_name, character.only = TRUE)) {
    cat(paste("Instalando o pacote:", pkg_name, "\n"))
    install_func()
    if (!require(pkg_name, character.only = TRUE)) {
      stop(paste("Não foi possível instalar ou carregar o pacote:", pkg_name))
    }
  } else {
    cat(paste("Pacote", pkg_name, "já está instalado.\n"))
  }
}

# 1. Pacotes do CRAN
install_if_missing("remotes", function() install.packages("remotes", repos = "http://cran.us.r-project.org"))
install_if_missing("WDI", function() install.packages("WDI", repos = "http://cran.us.r-project.org"))
install_if_missing("plm", function() install.packages("plm", repos = "http://cran.us.r-project.org"))
install_if_missing("dplyr", function() install.packages("dplyr", repos = "http://cran.us.r-project.org"))

# 2. Pacotes do GitHub
install_if_missing("freedomhouse", function() remotes::install_github("pachadotdev/freedomhouse"))
install_if_missing("wid", function() remotes::install_github("world-inequality-database/wid-r-tool"))


cat("\nInstalação de dependências concluída!\n")
