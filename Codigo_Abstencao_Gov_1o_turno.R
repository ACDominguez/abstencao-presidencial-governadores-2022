# Rode uma vez no Console, se ainda não instalou o pacote:
# install.packages("readxl")

# Carrega o pacote para importar arquivos Excel
library(readxl)

# Lê a aba "dados" como texto para aceitar vírgulas e sinais de porcentagem
dados <- read_excel(
  "abstencao_2022.xlsx",
  sheet = "dados",
  col_types = "text"
)

# Confere se a planilha contém as colunas usadas na análise
colunas_necessarias <- c(
  "uf", "abstencao_1o", "abstencao_2o", "eleicao_gov_1o"
)
if (!all(colunas_necessarias %in% names(dados))) {
  stop(paste(
    "A planilha precisa ter estas colunas:",
    paste(colunas_necessarias, collapse = ", ")
  ))
}

# Converte percentuais escritos como 22,43, 22.43 ou 22,43% em números
converter_percentual <- function(x) {
  texto <- trimws(as.character(x))
  texto <- gsub("%", "", texto, fixed = TRUE)
  texto <- gsub(",", ".", texto, fixed = TRUE)
  valor <- suppressWarnings(as.numeric(texto))
  validos <- valor[!is.na(valor)]
  
  # Se o Excel armazenou 22,43% como 0,2243, converte para 22,43
  if (length(validos) > 0 && max(validos) <= 1) {
    valor <- valor * 100
  }
  
  valor
}

# Aplica a conversão às duas colunas de abstenção
dados$abstencao_1o <- converter_percentual(dados$abstencao_1o)
dados$abstencao_2o <- converter_percentual(dados$abstencao_2o)

# Interrompe a análise se algum percentual estiver vazio ou inválido
if (anyNA(dados$abstencao_1o) || anyNA(dados$abstencao_2o)) {
  stop("Revise os valores nas colunas abstencao_1o e abstencao_2o.")
}

# Padroniza siglas e respostas da coluna do governador
dados$uf <- toupper(trimws(dados$uf))
dados$eleicao_gov_1o <- toupper(trimws(dados$eleicao_gov_1o))

# Exclui o DF, pois a pergunta é sobre os estados
dados_estados <- subset(dados, uf != "DF")

# Calcula a mudança e verifica se a abstenção aumentou
dados_estados$diferenca_pp <-
  dados_estados$abstencao_2o - dados_estados$abstencao_1o
dados_estados$aumentou <- ifelse(
  dados_estados$diferenca_pp > 0,
  "Aumentou",
  "Não aumentou"
)

# Forma os dois grupos da eleição para governador
dados_estados$grupo_governador <- ifelse(
  dados_estados$eleicao_gov_1o == "SIM",
  "Governador eleito no 1º turno",
  "Eleição para governador foi ao 2º turno"
)

# Conta quantos estados de cada grupo tiveram aumento ou não
tabela_contagens <- table(
  dados_estados$grupo_governador,
  dados_estados$aumentou
)
tabela_contagens

# Calcula, dentro de cada grupo, a porcentagem que aumentou/não aumentou
round(prop.table(tabela_contagens, margin = 1) * 100, 1)

# Calcula a variação média em pontos percentuais em cada grupo
aggregate(
  diferenca_pp ~ grupo_governador,
  data = dados_estados,
  FUN = mean
)
