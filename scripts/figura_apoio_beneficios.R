# =============================================================================
# Figura - Apoio público e benefícios obtidos com o uso de TDA
# CNAE 26 vs. média da indústria (tab1.4 e tab1.5_A)
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(here)

if (!exists("read_pintec_tema")) {
  source(here("scripts", "leer_pintec.R"))
}
if (!exists("tablas_cnae")) {
  tablas_cnae <- readRDS(here("data", "processed", "tablas_cnae.rds"))
}

# -----------------------------------------------------------------------------
# 1. Apoio público (tab1.5_A) -- % entre firmas adotantes de TDA
# -----------------------------------------------------------------------------

df_apoio <- tablas_cnae[["tab1.5_A"]]
util_col_apoio <- "numero_de_empresas_que_utilizaram_tecnologias_digitais_avancadas_total"
apoio_col <- names(df_apoio)[grepl("programa_de_apoio_publico", names(df_apoio))][1]

extrai_apoio <- function(mask, recorte) {
  r <- df_apoio[mask, ][1, ]
  tibble(
    categoria_indicador = "Utilizou programa de apoio público",
    recorte = recorte,
    pct = round(as.numeric(r[[apoio_col]]) / as.numeric(r[[util_col_apoio]]) * 100, 1)
  )
}

apoio_longo <- bind_rows(
  extrai_apoio(df_apoio$categoria == "Total Indústria", "Média da indústria"),
  extrai_apoio(grepl("informática", df_apoio$categoria), "CNAE 26")
)

# -----------------------------------------------------------------------------
# 2. Benefícios obtidos (tab1.4) -- % entre firmas adotantes de TDA
# -----------------------------------------------------------------------------

df_benef <- tablas_cnae[["tab1.4"]]
util_col_benef <- "numero_de_empresas_que_utilizaram_tecnologias_digitais_avancadas_total"

beneficio_sufixos <- c(
  "aumento_da_eficiencia" = "Aumento da eficiência",
  "reducao_do_impacto_ambiental" = "Redução do impacto ambiental",
  "melhoria_no_relacionamento_com_clientes_e_ou_fornecedores" = "Melhoria no relacionamento\ncom clientes/fornecedores",
  "maior_capacidade_de_desenvolvimento_de_produtos_ou_servicos_novos_ou_significativamente_aprimorados" = "Maior capacidade de desenv.\nde produtos/serviços novos",
  "maior_eficacia_no_atendimento_ao_mercado" = "Maior eficácia no\natendimento ao mercado",
  "maior_flexibilidade_em_processos_administrativos_produtivos_e_organizacionais" = "Maior flexibilidade\nem processos",
  "entrada_em_novos_mercados" = "Entrada em novos mercados"
  # "outros" excluído: categoria residual, pouco informativa
)

extrai_beneficio <- function(mask, recorte) {
  r <- df_benef[mask, ][1, ]
  util <- as.numeric(r[[util_col_benef]])
  map_dfr(names(beneficio_sufixos), function(sufixo) {
    col <- names(df_benef)[grepl(sufixo, names(df_benef), fixed = TRUE)][1]
    tibble(
      categoria_indicador = beneficio_sufixos[[sufixo]],
      pct = round(as.numeric(r[[col]]) / util * 100, 1)
    )
  }) %>%
    mutate(recorte = recorte)
}

beneficios_longo <- bind_rows(
  extrai_beneficio(df_benef$categoria == "Total Indústria", "Média da indústria"),
  extrai_beneficio(grepl("informática", df_benef$categoria), "CNAE 26")
)

# -----------------------------------------------------------------------------
# 4. Gráfico A -- Apoio público (pergunta institucional)
# -----------------------------------------------------------------------------

apoio_longo <- apoio_longo %>%
  mutate(recorte = factor(recorte, levels = c("CNAE 26", "Média da indústria")))

grafico_apoio <- ggplot(apoio_longo, aes(x = pct, y = recorte, fill = recorte)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = pct), hjust = -0.15, size = 4.5) +
  scale_fill_manual(values = c("CNAE 26" = "#c0392b", "Média da indústria" = "#7f8c8d"), guide = "none") +
  scale_x_continuous(limits = c(0, 35), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Utilização de programa de apoio público",
    subtitle = "% de firmas adotantes de TDA que utilizaram algum programa\nde apoio público voltado às TDA",
    x = "% de firmas adotantes de TDA", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold")
  )

print(grafico_apoio)

# -----------------------------------------------------------------------------
# 5. Gráfico B -- Benefícios obtidos (pergunta sobre resultados)
# -----------------------------------------------------------------------------

ordem_categorias <- beneficios_longo %>%
  filter(recorte == "CNAE 26") %>%
  arrange(desc(pct)) %>%
  pull(categoria_indicador)

beneficios_longo <- beneficios_longo %>%
  mutate(
    categoria_indicador = factor(categoria_indicador, levels = rev(ordem_categorias)),
    recorte = factor(recorte, levels = c("CNAE 26", "Média da indústria"))
  )

grafico_beneficios <- ggplot(beneficios_longo, aes(x = pct, y = categoria_indicador, fill = recorte)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = pct), position = position_dodge(width = 0.7),
            hjust = -0.15, size = 4) +
  scale_fill_manual(values = c("CNAE 26" = "#c0392b", "Média da indústria" = "#7f8c8d"), name = NULL) +
  scale_x_continuous(limits = c(0, 105), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Benefícios obtidos com o uso de TDA",
    subtitle = "% de firmas adotantes de TDA, CNAE 26 vs. média da indústria",
    x = "% de firmas adotantes de TDA", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10)
  )

print(grafico_beneficios)

# -----------------------------------------------------------------------------
# 6. Salvar
# -----------------------------------------------------------------------------

dir.create(here("output"), showWarnings = FALSE)

ggsave(here("output", "figura_apoio_publico.png"), grafico_apoio, width = 10, height = 4.2, dpi = 300)
ggsave(here("output", "figura_beneficios_obtidos.png"), grafico_beneficios, width = 10, height = 7, dpi = 300)

write.csv(apoio_longo, here("data", "processed", "apoio_publico_cnae26_vs_media.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(beneficios_longo, here("data", "processed", "beneficios_obtidos_cnae26_vs_media.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")