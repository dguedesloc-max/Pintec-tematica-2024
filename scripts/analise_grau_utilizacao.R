library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(here)

# ---------------------------------------------------------------------
# Carga tablas_cnae ya procesada
# ---------------------------------------------------------------------

if (!exists("tablas_cnae")) {
  tablas_cnae <- readRDS(here("data", "processed", "tablas_cnae.rds"))
}

sheets_tec <- c(
  "tab1.3.1" = "Big Data",
  "tab1.3.2" = "Cloud",
  "tab1.3.3" = "IA",
  "tab1.3.4" = "IoT",
  "tab1.3.5" = "Manufatura aditiva",
  "tab1.3.6" = "Robótica"
)

funcoes_paper <- c("administracao", "producao", "projeto")

# ---------------------------------------------------------------------
# % de uso abrangente ENTRE QUEM JÁ ADOTOU (não sobre o total de firmas
# do setor) -- abrangente / utilizou * 100. Esta é a base correta para o
# objetivo (ii): "mesmo entre firmas que já adotaram a tecnologia".
# ---------------------------------------------------------------------

extrair_grau_utilizacao <- function(df, tecnologia) {
  
  extrai_linha <- function(mask, recorte) {
    linha <- df[mask, ][1, ]
    map_dfr(funcoes_paper, function(f) {
      util_col <- names(df)[str_ends(names(df), paste0("_", f, "_utilizou"))][1]
      abr_col <- names(df)[str_ends(names(df), paste0("_", f, "_abrangente"))][1]
      util <- as.numeric(linha[[util_col]])
      abr <- as.numeric(linha[[abr_col]])
      tibble(
        funcao = f,
        pct_abrangente = if (util > 0) round(abr / util * 100, 1) else NA_real_
      )
    }) %>%
      mutate(recorte = recorte)
  }
  
  bind_rows(
    extrai_linha(df$categoria == "Total Indústria", "media_industria"),
    extrai_linha(str_detect(df$categoria, "informática"), "cnae_26")
  ) %>%
    mutate(tecnologia = tecnologia, .before = 1)
}

grau_utilizacao_longa <- imap_dfr(sheets_tec, function(tecnologia, sheet) {
  extrair_grau_utilizacao(tablas_cnae[[sheet]], tecnologia)
})

# ---------------------------------------------------------------------
# Formato ancho: una fila por tecnologia x recorte
# ---------------------------------------------------------------------

grau_utilizacao_larga <- grau_utilizacao_longa %>%
  pivot_wider(names_from = funcao, values_from = pct_abrangente) %>%
  arrange(tecnologia, recorte)

print(grau_utilizacao_larga, n = Inf)

# ---------------------------------------------------------------------
# "Aprofundamento" no CNAE 26 vs. média da indústria: quanto maior/menor
# é o % de uso abrangente em CNAE 26, para cada função -- em p.p.
# Valor positivo = CNAE 26 usa de forma mais abrangente que a média,
# entre quem já adotou.
# ---------------------------------------------------------------------

aprofundamento_cnae26 <- grau_utilizacao_larga %>%
  pivot_longer(cols = all_of(funcoes_paper), names_to = "funcao", values_to = "pct_abrangente") %>%
  pivot_wider(names_from = recorte, values_from = pct_abrangente) %>%
  mutate(diferenca_cnae26_media_pp = round(cnae_26 - media_industria, 1)) %>%
  arrange(funcao, tecnologia)

print(aprofundamento_cnae26, n = Inf)

# ---------------------------------------------------------------------
# Assimetria no grau de utilização entre funções (paralelo ao que já
# calculamos para adoção): dentro de cada recorte, quanto o % de uso
# abrangente de uma função supera o de outra.
# ---------------------------------------------------------------------

assimetria_grau_utilizacao <- grau_utilizacao_larga %>%
  mutate(
    abrangente_admin_producao_pp = round(administracao - producao, 1),
    abrangente_admin_projeto_pp = round(administracao - projeto, 1),
    abrangente_projeto_producao_pp = round(projeto - producao, 1)
  )

print(
  assimetria_grau_utilizacao %>%
    select(tecnologia, recorte, starts_with("abrangente_")),
  n = Inf
)

# ---------------------------------------------------------------------
# Guardar
# ---------------------------------------------------------------------

write.csv(grau_utilizacao_larga, here("data", "processed", "grau_utilizacao_administracao_producao_projeto.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(aprofundamento_cnae26, here("data", "processed", "aprofundamento_grau_utilizacao_cnae26.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(assimetria_grau_utilizacao, here("data", "processed", "assimetria_grau_utilizacao.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ejemplos de uso posterior:
# aprofundamento_cnae26 %>% filter(tecnologia == "IA")
# assimetria_grau_utilizacao %>% filter(recorte == "cnae_26") %>% arrange(abrangente_projeto_producao_pp)