library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(here)

# ---------------------------------------------------------------------
# Carga tablas_cnae ya procesada (correr leer_pintec.R antes, una vez,
# si todavía no existe el .rds)
# ---------------------------------------------------------------------

tablas_cnae <- readRDS(here("data", "processed", "tablas_cnae.rds"))

# ---------------------------------------------------------------------
# Mapeo hoja -> nombre de tecnologia (para las 6 hojas de adoção por TDA
# específica, tab1.3.1 a tab1.3.6)
# ---------------------------------------------------------------------

sheets_tec <- c(
  "tab1.3.1" = "Big Data",
  "tab1.3.2" = "Cloud",
  "tab1.3.3" = "IA",
  "tab1.3.4" = "IoT",
  "tab1.3.5" = "Manufatura aditiva",
  "tab1.3.6" = "Robótica"
)

funcoes <- c("projeto", "producao", "logistica", "administracao", "comercializacao")

#' Extrae el % de firmas que utilizam a tecnologia em cada função de negócio,
#' para dos recortes: média da indústria (Total Indústria, tal como
#' publicado pela PINTEC) e CNAE 26.
#'
#' % = utilizou_na_funcao / total_de_empresas -- misma base que citás en
#' el pre-proyecto (ej. "63,7% das empresas do CNAE 26 utilizam IA em
#' Administração").
extrair_assimetria <- function(df, tecnologia) {
  total_col <- "numero_de_empresas_total"
  
  extrai_linha <- function(mask, recorte) {
    linha <- df[mask, ][1, ]
    map_dfr(funcoes, function(f) {
      col <- names(df)[str_ends(names(df), paste0("_", f, "_utilizou"))][1]
      tibble(
        funcao = f,
        pct = round(as.numeric(linha[[col]]) / as.numeric(linha[[total_col]]) * 100, 1)
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

# ---------------------------------------------------------------------
# Rodar para las 6 tecnologías y consolidar
# ---------------------------------------------------------------------

assimetria_longa <- imap_dfr(sheets_tec, function(tecnologia, sheet) {
  extrair_assimetria(tablas_cnae[[sheet]], tecnologia)
})

# ---------------------------------------------------------------------
# Formato ancho: una fila por tecnologia x recorte, una columna por função,
# más la asimetria Administração - Produção en puntos porcentuales
# ---------------------------------------------------------------------

assimetria_larga <- assimetria_longa %>%
  pivot_wider(names_from = funcao, values_from = pct) %>%
  mutate(
    asimetria_admin_producao_pp = round(administracao - producao, 1)
  ) %>%
  arrange(tecnologia, recorte)

print(assimetria_larga, n = Inf)

# ---------------------------------------------------------------------
# Guardar
# ---------------------------------------------------------------------

write.csv(
  assimetria_larga,
  here("data", "processed", "assimetria_administracao_producao.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ejemplo de uso posterior:
# assimetria_larga %>% filter(tecnologia == "IA")
# assimetria_larga %>% filter(recorte == "cnae_26") %>% arrange(asimetria_admin_producao_pp)