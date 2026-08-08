library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(here)

# ---------------------------------------------------------------------
# Este script depende de las funciones read_pintec_tema() y
# simplify_pintec_names() definidas en leer_pintec.R.
# ---------------------------------------------------------------------

if (!exists("read_pintec_tema")) {
  source(here("scripts", "leer_pintec.R"))
}
if (!exists("tablas_cnae")) {
  tablas_cnae <- readRDS(here("data", "processed", "tablas_cnae.rds"))
}

path_cv <- here("data", "raw", "PINTEC_Semestral_2024_Indicadores_Tematicos_CNAE_coeficiente_de_variacao.xlsx")

sheets_tec <- c(
  "tab1.3.1" = "Big Data",
  "tab1.3.2" = "Cloud",
  "tab1.3.3" = "IA",
  "tab1.3.4" = "IoT",
  "tab1.3.5" = "Manufatura aditiva",
  "tab1.3.6" = "Robótica"
)

tablas_cv <- purrr::map(names(sheets_tec), ~ read_pintec_tema(path_cv, .x)) %>%
  setNames(names(sheets_tec))

# As três funções de negócio consideradas no paper (decisão do orientador):
# Administração, Produção e Desenvolvimento de projetos.
funcoes_paper <- c("administracao", "producao", "projeto")

# ---------------------------------------------------------------------
# Extrae, para las 24 divisões, el % de adoção em cada una de las 3
# funções, más su CV, para una tecnologia dada.
# ---------------------------------------------------------------------

extrair_distribuicao <- function(df, df_cv, tecnologia) {
  total_col <- "numero_de_empresas_total"
  divisoes <- df %>% filter(nivel == "categoria_detalhe")
  divisoes_cv <- df_cv %>% filter(nivel == "categoria_detalhe")
  
  base <- divisoes %>% select(categoria)
  
  for (f in funcoes_paper) {
    col <- names(df)[str_ends(names(df), paste0("_", f, "_utilizou"))][1]
    cv_col <- names(df_cv)[str_ends(names(df_cv), paste0("_", f, "_utilizou"))][1]
    
    base[[f]] <- round(divisoes[[col]] / divisoes[[total_col]] * 100, 1)
    base[[paste0("cv_", f)]] <- divisoes_cv[[cv_col]][match(base$categoria, divisoes_cv$categoria)]
  }
  
  base %>%
    mutate(
      tecnologia = tecnologia,
      asimetria_admin_producao_pp = round(administracao - producao, 1),
      asimetria_admin_projeto_pp = round(administracao - projeto, 1),
      asimetria_projeto_producao_pp = round(projeto - producao, 1),
      .before = 1
    )
}

heterogeneidade_setorial <- imap_dfr(sheets_tec, function(tecnologia, sheet) {
  extrair_distribuicao(tablas_cnae[[sheet]], tablas_cv[[sheet]], tecnologia)
})

# ---------------------------------------------------------------------
# Formato longo: una fila por tecnologia x divisão x par de funções
# ---------------------------------------------------------------------

pares <- c(
  "asimetria_admin_producao_pp" = "admin_producao",
  "asimetria_admin_projeto_pp" = "admin_projeto",
  "asimetria_projeto_producao_pp" = "projeto_producao"
)

heterogeneidade_longa <- heterogeneidade_setorial %>%
  select(tecnologia, categoria, all_of(names(pares))) %>%
  pivot_longer(cols = all_of(names(pares)), names_to = "par_raw", values_to = "asimetria_pp") %>%
  mutate(par = pares[par_raw]) %>%
  select(-par_raw)

# ---------------------------------------------------------------------
# Media, desvío padrão, min e max de cada par, entre las 24 divisões
# ---------------------------------------------------------------------

resumo_heterogeneidade <- heterogeneidade_longa %>%
  group_by(tecnologia, par) %>%
  summarise(
    media_asimetria = round(mean(asimetria_pp), 1),
    dp_asimetria = round(sd(asimetria_pp), 1),
    min_asimetria = min(asimetria_pp),
    max_asimetria = max(asimetria_pp),
    .groups = "drop"
  )

# ---------------------------------------------------------------------
# Posición de CNAE 26 dentro de cada distribución: z-score y percentil
# ---------------------------------------------------------------------

cv_por_funcao <- heterogeneidade_setorial %>%
  filter(str_detect(categoria, "informática")) %>%
  select(tecnologia, cv_administracao, cv_producao, cv_projeto)

cnae26_posicao <- heterogeneidade_longa %>%
  filter(str_detect(categoria, "informática")) %>%
  left_join(resumo_heterogeneidade, by = c("tecnologia", "par")) %>%
  mutate(z_score = round((asimetria_pp - media_asimetria) / dp_asimetria, 2))

cnae26_posicao$percentil <- pmap_dbl(
  list(cnae26_posicao$tecnologia, cnae26_posicao$par, cnae26_posicao$asimetria_pp),
  function(tec, p, valor) {
    dist <- heterogeneidade_longa %>%
      filter(tecnologia == tec, par == p) %>%
      pull(asimetria_pp)
    round(mean(dist <= valor) * 100, 0)
  }
)

# aviso de confiabilidade: CV alto (>25%) o ausente en cualquiera de las
# dos funções que componen el par
cnae26_posicao <- cnae26_posicao %>%
  left_join(cv_por_funcao, by = "tecnologia") %>%
  mutate(
    aviso_cv = case_when(
      par == "admin_producao" & (is.na(cv_administracao) | is.na(cv_producao)) ~ "Sem dado",
      par == "admin_producao" & (cv_administracao > 25 | cv_producao > 25) ~ "CV alto",
      par == "admin_projeto" & (is.na(cv_administracao) | is.na(cv_projeto)) ~ "Sem dado",
      par == "admin_projeto" & (cv_administracao > 25 | cv_projeto > 25) ~ "CV alto",
      par == "projeto_producao" & (is.na(cv_projeto) | is.na(cv_producao)) ~ "Sem dado",
      par == "projeto_producao" & (cv_projeto > 25 | cv_producao > 25) ~ "CV alto",
      TRUE ~ "OK"
    )
  )

print(
  cnae26_posicao %>%
    select(tecnologia, par, asimetria_pp, media_asimetria, dp_asimetria, z_score, percentil, aviso_cv) %>%
    arrange(par, tecnologia),
  n = Inf
)

# ---------------------------------------------------------------------
# Guardar
# ---------------------------------------------------------------------

write.csv(heterogeneidade_setorial, here("data", "processed", "heterogeneidade_setorial_24_divisoes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(heterogeneidade_longa, here("data", "processed", "heterogeneidade_setorial_longa.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cnae26_posicao, here("data", "processed", "cnae26_posicao_distribuicao.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ejemplos de uso posterior:
# cnae26_posicao %>% filter(par == "projeto_producao") %>% arrange(desc(abs(z_score)))
# heterogeneidade_longa %>% filter(tecnologia == "IA", par == "projeto_producao")