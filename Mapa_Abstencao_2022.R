# Rode uma vez no Console, se algum pacote ainda não estiver instalado:
# install.packages(c("readxl", "ggplot2", "brazilmaps", "ggpattern"))

# Carrega os pacotes
library(readxl)     # importa a planilha Excel
library(ggplot2)    # desenha o mapa
library(brazilmaps) # fornece a malha estadual
library(ggpattern)  # fornece preenchimentos com padrões

# Importa a aba "dados"; texto evita que células mudem o tipo da coluna
dados <- read_excel(
  "abstencao_2022.xlsx",
  sheet = "dados",
  col_types = "text"
)

# Confere se a planilha contém as colunas necessárias
colunas_necessarias <- c(
  "uf", "abstencao_1o", "abstencao_2o", "eleicao_gov_1o"
)
if (!all(colunas_necessarias %in% names(dados))) {
  stop(paste(
    "A planilha precisa ter estas colunas:",
    paste(colunas_necessarias, collapse = ", ")
  ))
}

# Converte percentuais com ponto, vírgula ou símbolo % em números
converter_percentual <- function(x) {
  texto <- trimws(as.character(x))
  texto <- gsub("%", "", texto, fixed = TRUE)
  texto <- gsub(",", ".", texto, fixed = TRUE)
  valor <- suppressWarnings(as.numeric(texto))
  validos <- valor[!is.na(valor)]
  
  # Excel pode guardar 22,43% como 0,2243; converte isso para 22,43
  if (length(validos) > 0 && max(validos) <= 1) {
    valor <- valor * 100
  }
  
  valor
}

# Converte as duas colunas de abstenção
dados$abstencao_1o <- converter_percentual(dados$abstencao_1o)
dados$abstencao_2o <- converter_percentual(dados$abstencao_2o)

# Interrompe se houver percentual vazio ou inválido
linhas_invalidas <- which(
  is.na(dados$abstencao_1o) | is.na(dados$abstencao_2o)
)
if (length(linhas_invalidas) > 0) {
  stop(paste(
    "Revise os percentuais nas linhas do Excel:",
    paste(linhas_invalidas + 1, collapse = ", ")
  ))
}

# Padroniza as siglas e as respostas Sim/Não
dados$uf <- toupper(trimws(dados$uf))
dados$eleicao_gov_1o <- toupper(trimws(dados$eleicao_gov_1o))

# Verifica se a coluna informa Sim ou Não para todas as UFs
if (anyNA(dados$eleicao_gov_1o) ||
    !all(dados$eleicao_gov_1o %in% c("SIM", "NÃO", "NAO"))) {
  stop("Preencha eleicao_gov_1o com Sim ou Não para todas as UFs.")
}

# Calcula a diferença em pontos percentuais: 2º turno menos 1º turno
dados$diferenca_pp <- dados$abstencao_2o - dados$abstencao_1o

# Identifica o turno em que terminou a eleição para governador
dados$grupo_governador <- ifelse(
  dados$eleicao_gov_1o == "SIM",
  "Governador eleito no 1º turno",
  "Eleição para governador foi ao 2º turno"
)

# Obtém a malha estadual incluída no brazilmaps
estados <- brazilmaps::get_brmap("state")

# Confere se todos os estados e o DF constam na planilha
faltam <- setdiff(estados$state_abbreviation, dados$uf)
if (length(faltam) > 0) {
  stop(paste("Faltam estas UFs na planilha:", paste(faltam, collapse = ", ")))
}

# Junta a planilha à malha pela sigla da UF
mapa_dados <- brazilmaps::join_brmap(
  estados,
  dados,
  by = c("state_abbreviation" = "uf")
)

# Formata a diferença com sinal e duas casas decimais
mapa_dados$rotulo_diferenca <- formatC(
  mapa_dados$diferenca_pp,
  format = "f",
  digits = 2,
  flag = "+",
  decimal.mark = ","
)

# Desenha o mapa; cor = diferença e listras = governador eleito no 1º turno
mapa <- ggplot(mapa_dados) +
  ggpattern::geom_sf_pattern(
    aes(fill = diferenca_pp, pattern = grupo_governador),
    color = "white",
    linewidth = 0.25,
    pattern_angle = 45,
    pattern_density = 0.05,
    pattern_spacing = 0.035,
    pattern_size = 0.3,
    pattern_fill = "#222222",
    pattern_colour = "#222222"
  ) +
  # Exibe as siglas menores, acima dos números
  geom_sf_text(
    aes(label = state_abbreviation),
    size = 2.1,
    nudge_y = 0.15,
    color = "black"
  ) +
  # Exibe as diferenças maiores e em negrito
  geom_sf_text(
    aes(label = rotulo_diferenca),
    size = 5,
    fontface = "bold",
    nudge_y = -0.15,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "Diferença da abstenção\n(pontos percentuais)"
  ) +
  ggpattern::scale_pattern_manual(
    values = c(
      "Governador eleito no 1º turno" = "stripe",
      "Eleição para governador foi ao 2º turno" = "none"
    ),
    name = "Eleição para governador"
  ) +
  labs(
    title = "Abstenção presidencial e eleição para governador — 2022",
    subtitle = "Listras indicam governador eleito no 1º turno",
    caption = paste(
      "Diferença = 2º turno − 1º turno; valores positivos indicam aumento.",
      "Fonte: TSE; malha: IBGE, via brazilmaps."
    )
  ) +
  theme_void() +
  theme(legend.position = "bottom")

# Exibe explicitamente o gráfico na aba Plots ao rodar o Script inteiro
print(mapa)

# Salva o PNG na pasta de trabalho
ggsave("mapa_abstencao_governador_2022.png", mapa,
       width = 12, height = 8, dpi = 300)
