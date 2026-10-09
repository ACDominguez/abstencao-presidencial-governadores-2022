# Infográfico: abstenção presidencial e turno da eleição para governador — 2022
# Este script lê a planilha, calcula os indicadores e exporta um PNG.
# Pressupõe um arquivo abstencao_2022.xlsx com uma aba "dados" e as colunas:
# uf, abstencao_1o, abstencao_2o, eleicao_gov_1o

# Rode uma vez no Console, se o pacote ainda não estiver instalado:
# install.packages("readxl")

# Carrega o pacote que lê arquivos Excel
library(readxl)

# Lê a planilha como texto para aceitar vírgula decimal e o símbolo %
dados <- read_excel(
  "abstencao_2022.xlsx",
  sheet = "dados",
  col_types = "text"
)

# Confere se a planilha tem todas as colunas necessárias
colunas_necessarias <- c(
  "uf", "abstencao_1o", "abstencao_2o", "eleicao_gov_1o"
)
if (!all(colunas_necessarias %in% names(dados))) {
  stop(paste(
    "A planilha precisa ter estas colunas:",
    paste(colunas_necessarias, collapse = ", ")
  ))
}

# Converte percentuais com vírgula/ponto decimal e com ou sem símbolo %
converter_percentual <- function(x) {
  texto <- trimws(as.character(x))
  texto <- gsub("%", "", texto, fixed = TRUE)
  texto <- gsub(",", ".", texto, fixed = TRUE)
  valor <- suppressWarnings(as.numeric(texto))
  valores_validos <- valor[!is.na(valor)]

  # Excel pode armazenar 22,43% como 0,2243; converte para escala 0–100
  if (length(valores_validos) > 0 && max(valores_validos) <= 1) {
    valor <- valor * 100
  }

  valor
}

# Converte as duas colunas de abstenção para números
dados$abstencao_1o <- converter_percentual(dados$abstencao_1o)
dados$abstencao_2o <- converter_percentual(dados$abstencao_2o)

# Padroniza as siglas e a resposta sobre o turno da eleição para governador
dados$uf <- toupper(trimws(dados$uf))
dados$eleicao_gov_1o <- toupper(trimws(dados$eleicao_gov_1o))

# Interrompe o script se faltarem percentuais ou respostas válidas
if (anyNA(dados$abstencao_1o) || anyNA(dados$abstencao_2o)) {
  stop("Revise os valores de abstencao_1o e abstencao_2o na planilha.")
}
if (any(!dados$eleicao_gov_1o %in% c("SIM", "NÃO", "NAO"))) {
  stop("Preencha eleicao_gov_1o com Sim ou Não para cada UF.")
}
if (anyDuplicated(dados$uf)) {
  stop("Há UFs repetidas na planilha; mantenha apenas uma linha por UF.")
}

# Exclui o Distrito Federal: não há eleição para governador no DF
dados_estados <- subset(dados, uf != "DF")
if (nrow(dados_estados) != 26) {
  stop(paste(
    "A análise espera 26 estados após excluir o DF, mas encontrou",
    nrow(dados_estados), "linhas. Confira a planilha."
  ))
}

# Calcula a diferença entre os turnos e identifica se ela é positiva
dados_estados$diferenca_pp <-
  dados_estados$abstencao_2o - dados_estados$abstencao_1o
dados_estados$aumentou <- dados_estados$diferenca_pp > 0

# Separa os estados conforme o turno em que se definiu a eleição para governador
grupo_primeiro <- subset(dados_estados, eleicao_gov_1o == "SIM")
grupo_segundo <- subset(dados_estados, eleicao_gov_1o %in% c("NÃO", "NAO"))

# Calcula quantidade e proporção de estados com aumento em cada grupo
n_primeiro <- nrow(grupo_primeiro)
n_segundo <- nrow(grupo_segundo)
aumentos_primeiro <- sum(grupo_primeiro$aumentou)
aumentos_segundo <- sum(grupo_segundo$aumentou)
sem_aumento_primeiro <- n_primeiro - aumentos_primeiro
sem_aumento_segundo <- n_segundo - aumentos_segundo
pct_primeiro <- 100 * aumentos_primeiro / n_primeiro
pct_segundo <- 100 * aumentos_segundo / n_segundo
diferenca_pct <- pct_primeiro - pct_segundo

# Lista as siglas dos estados em que a abstenção aumentou, em cada grupo
ufs_aumento_primeiro <- sort(grupo_primeiro$uf[grupo_primeiro$aumentou])
ufs_aumento_segundo <- sort(grupo_segundo$uf[grupo_segundo$aumentou])
texto_ufs <- paste0(
  "Com aumento: 1º turno — ", paste(ufs_aumento_primeiro, collapse = ", "),
  "   |   2º turno — ", paste(ufs_aumento_segundo, collapse = ", ")
)

# Mostra no Console os números usados no infográfico
cat("Estados com governador eleito no 1º turno:", aumentos_primeiro,
    "de", n_primeiro, "(", round(pct_primeiro, 1), "% )\n")
cat("Estados cuja eleição para governador foi ao 2º turno:", aumentos_segundo,
    "de", n_segundo, "(", round(pct_segundo, 1), "% )\n")
cat("Diferença entre as proporções:", round(diferenca_pct, 1), "pontos percentuais\n")

# Carrega o sistema gráfico base do R
grid::grid.newpage()

# Paleta de cores do infográfico
cor_fundo <- "#F4F1E8"
cor_texto <- "#172B3A"
cor_secundaria <- "#52626D"
cor_verde_azulado <- "#087E83"
cor_laranja <- "#D27632"
cor_trilho <- "#E6E1D6"
cor_branco <- "#FFFEFB"

# Desenha o infográfico numa página usando coordenadas proporcionais (0 a 1)
desenhar_infografico <- function() {
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = cor_fundo, col = NA))

  # Rótulo superior e título
  grid::grid.text(
    "ELEIÇÕES BRASILEIRAS · 2022",
    x = 0.08, y = 0.945, just = c("left", "centre"),
    gp = grid::gpar(col = cor_verde_azulado, fontsize = 10, fontface = "bold")
  )
  grid::grid.text(
    "A abstenção subiu mais\nquando o governador foi eleito no 1º turno?",
    x = 0.08, y = 0.895, just = c("left", "top"),
    gp = grid::gpar(col = cor_texto, fontsize = 21, fontface = "bold", lineheight = 1.15)
  )
  grid::grid.text(
    "Comparação entre o turno decisivo para governador e a variação\nda abstenção na eleição presidencial entre o 1º e o 2º turno.",
    x = 0.08, y = 0.815, just = c("left", "top"),
    gp = grid::gpar(col = cor_secundaria, fontsize = 11, lineheight = 1.25)
  )

  # Faixa principal com a diferença entre os grupos
  grid::grid.roundrect(
    x = 0.50, y = 0.717, width = 0.84, height = 0.055,
    r = grid::unit(0.012, "snpc"),
    gp = grid::gpar(fill = cor_texto, col = NA)
  )
  diferenca_formatada <- paste0(
    ifelse(diferenca_pct > 0, "+", ""),
    formatC(diferenca_pct, format = "f", digits = 1, decimal.mark = ","),
    " p.p."
  )
  grid::grid.text(
    diferenca_formatada,
    x = 0.11, y = 0.717, just = c("left", "centre"),
    gp = grid::gpar(col = "#F2C66D", fontsize = 20, fontface = "bold")
  )
  grid::grid.text(
    "de diferença na proporção de estados com aumento",
    x = 0.36, y = 0.717, just = c("left", "centre"),
    gp = grid::gpar(col = cor_branco, fontsize = 10.5)
  )

  # Desenha um cartão de comparação para cada grupo
  desenhar_cartao <- function(x, cor, titulo, n_estados, n_aumentos,
                              n_sem_aumento, percentual) {
    y <- 0.405
    largura <- 0.405
    altura <- 0.275

    # Fundo do cartão e faixa colorida superior
    grid::grid.roundrect(
      x = x + largura / 2, y = y + altura / 2,
      width = largura, height = altura,
      r = grid::unit(0.015, "snpc"),
      gp = grid::gpar(fill = cor_branco, col = "#DDD7CB", lwd = 0.8)
    )
    grid::grid.roundrect(
      x = x + largura / 2, y = y + altura - 0.014,
      width = largura - 0.02, height = 0.012,
      r = grid::unit(0.006, "snpc"),
      gp = grid::gpar(fill = cor, col = NA)
    )

    # Título, número de estados, percentual e contagem
    grid::grid.text(
      titulo,
      x = x + 0.025, y = y + altura - 0.045,
      just = c("left", "top"),
      gp = grid::gpar(col = cor_texto, fontsize = 11.5,
                      fontface = "bold", lineheight = 1.1)
    )
    grid::grid.text(
      paste(n_estados, "estados"),
      x = x + 0.025, y = y + altura - 0.095,
      just = c("left", "top"),
      gp = grid::gpar(col = cor_secundaria, fontsize = 9.5)
    )
    grid::grid.text(
      paste0(formatC(percentual, format = "f", digits = 1,
                     decimal.mark = ","), "%"),
      x = x + 0.025, y = y + altura - 0.155,
      just = c("left", "top"),
      gp = grid::gpar(col = cor, fontsize = 27, fontface = "bold")
    )
    grid::grid.text(
      paste(n_aumentos, "de", n_estados, "tiveram aumento"),
      x = x + 0.025, y = y + altura - 0.198,
      just = c("left", "top"),
      gp = grid::gpar(col = cor_texto, fontsize = 9.5)
    )

    # Barra que visualiza a proporção com aumento
    x_barra <- x + 0.025
    y_barra <- y + 0.042
    largura_barra <- largura - 0.05
    grid::grid.roundrect(
      x = x_barra + largura_barra / 2, y = y_barra + 0.009,
      width = largura_barra, height = 0.018,
      r = grid::unit(0.008, "snpc"),
      gp = grid::gpar(fill = cor_trilho, col = NA)
    )
    grid::grid.roundrect(
      x = x_barra + largura_barra * percentual / 200,
      y = y_barra + 0.009,
      width = largura_barra * percentual / 100, height = 0.018,
      r = grid::unit(0.008, "snpc"),
      gp = grid::gpar(fill = cor, col = NA)
    )
    grid::grid.text(
      paste(n_aumentos, "aumentaram  · ", n_sem_aumento,
            "não aumentaram"),
      x = x_barra, y = y_barra - 0.017,
      just = c("left", "top"),
      gp = grid::gpar(col = cor_secundaria, fontsize = 7.8)
    )
  }

  desenhar_cartao(
    x = 0.08,
    cor = cor_verde_azulado,
    titulo = "Governador eleito\nno 1º turno",
    n_estados = n_primeiro,
    n_aumentos = aumentos_primeiro,
    n_sem_aumento = sem_aumento_primeiro,
    percentual = pct_primeiro
  )
  desenhar_cartao(
    x = 0.515,
    cor = cor_laranja,
    titulo = "Eleição para governador\nfoi ao 2º turno",
    n_estados = n_segundo,
    n_aumentos = aumentos_segundo,
    n_sem_aumento = sem_aumento_segundo,
    percentual = pct_segundo
  )

  # Caixa de interpretação
  grid::grid.roundrect(
    x = 0.50, y = 0.2825, width = 0.84, height = 0.155,
    r = grid::unit(0.015, "snpc"),
    gp = grid::gpar(fill = "#E9E5DA", col = NA)
  )
  grid::grid.text(
    "O que isso sugere",
    x = 0.11, y = 0.328, just = c("left", "top"),
    gp = grid::gpar(col = cor_texto, fontsize = 11.5, fontface = "bold")
  )
  interpretacao <- paste0(
    "As proporções são próximas: ",
    formatC(pct_primeiro, format = "f", digits = 1, decimal.mark = ","),
    "% contra ",
    formatC(pct_segundo, format = "f", digits = 1, decimal.mark = ","),
    "%. A diferença é pequena;\nesta comparação descritiva não mostra que um resultado causou o outro."
  )
  grid::grid.text(
    interpretacao,
    x = 0.11, y = 0.292, just = c("left", "top"),
    gp = grid::gpar(col = cor_texto, fontsize = 9.5, lineheight = 1.25)
  )
  grid::grid.text(
    texto_ufs,
    x = 0.11, y = 0.238, just = c("left", "top"),
    gp = grid::gpar(col = cor_secundaria, fontsize = 7.8)
  )

  # Rodapé com definição, escopo e fonte
  grid::grid.text(
    "Como ler: aumento significa abstenção presidencial no 2º turno maior que no 1º turno.",
    x = 0.08, y = 0.155, just = c("left", "top"),
    gp = grid::gpar(col = cor_texto, fontsize = 8.5)
  )
  grid::grid.text(
    "Base: 26 estados; DF excluído porque não há eleição para governador. Abstenção como percentual do eleitorado apto.",
    x = 0.08, y = 0.118, just = c("left", "top"),
    gp = grid::gpar(col = cor_secundaria, fontsize = 7.5)
  )
  grid::grid.text(
    "Fonte: TSE. Percentuais arredondados a duas casas na planilha; cálculo por estado.",
    x = 0.08, y = 0.082, just = c("left", "top"),
    gp = grid::gpar(col = cor_secundaria, fontsize = 7.5)
  )
}

# Mostra o infográfico na aba Plots do RStudio
desenhar_infografico()

# Salva uma cópia em PNG na pasta de trabalho
png(
  filename = "infografico_abstencao_governador_2022.png",
  width = 1800,
  height = 2160,
  res = 180,
  bg = cor_fundo
)
desenhar_infografico()
dev.off()
