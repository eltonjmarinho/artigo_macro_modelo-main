# Análise de Dados: Liberdade Econômica, Desigualdade e Crescimento

**Evidências Empíricas para uma Amostra Global (2000–2023)**

---

## Contexto do Projeto

Este repositório contém os scripts e dados utilizados na análise para o artigo "Liberdade Econômica, Desigualdade e Crescimento: Evidências Empíricas para uma Amostra Global (2000–2023)".

Trabalho apresentado ao Programa de Pós-Graduação em Economia da Universidade Federal da Paraíba (UFPB), em cumprimento às exigências da disciplina de Macroeconomia I, ministrada pelo professor Dr. Edilean Kleber da Silva Bejarano Aragon.

### Autores

*   Elton John Marinho de Lima
*   Raphael Lopes Monteiro

---

## Estrutura do Projeto

O projeto segue uma estrutura baseada no padrão MVC (Model-View-Controller) para organizar a análise de dados:

*   `/data`: Armazena os dados brutos e processados.
*   `/models`: Contém a lógica de análise e modelagem estatística.
*   `/views`: Para visualizações, gráficos e tabelas.
*   `/controllers`: Lógica principal que conecta os modelos e as visualizações.
*   `/scripts`: Scripts para download e preparação dos dados.

---

## Como Usar

### 1. Instalar Dependências

Antes de executar os scripts, instale todos os pacotes R necessários executando o seguinte script no seu console R:

```R
source('install_dependencies.R')
```

### 2. Baixar os Dados

Os dados são baixados de diferentes fontes usando os scripts na pasta `/scripts`. Execute-os na ordem desejada para popular a pasta `/data`:

```R
# Baixa dados do Fraser Institute (via pacote 'freedomhouse')
source('scripts/download_data_fraser_freedom.R')

# Baixa dados do World Inequality Database (WID)
source('scripts/download_data_wid.R')

# Baixa dados do World Development Indicators (WDI)
source('scripts/download_data_wdi.R')
```
**Nota:** Os scripts para WID e WDI são modelos. Você precisará editá-los para especificar os indicadores e períodos exatos que deseja baixar.
