library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

if (!exists("grau_utilizacao_larga")) {
  source(here("scripts", "analise_grau_utilizacao.R"))
}

# ---------------------------------------------------------------------
# Formato longo para o gráfico: uma linha por tecnologia x recorte x função
# ---------------------------------------------------------------------

dados_grafico <- grau_utilizacao_larga %>%
  pivot_longer(cols = c(administracao, producao, projeto),
               names_to = "funcao", values_to = "pct_abrangente") %>%
  mutate(
    funcao = recode(funcao,
                    administracao = "Administração",
                    producao = "Produção",
                    projeto = "Desenv. de projetos"),
    funcao = factor(funcao, levels = c("Administração", "Desenv. de projetos", "Produção")),
    recorte = recode(recorte, cnae_26 = "CNAE 26", media_industria = "Média da indústria"),
    recorte = factor(recorte, levels = c("CNAE 26", "Média da indústria"))
  )

grafico_grau_utilizacao <- ggplot(dados_grafico, aes(x = funcao, y = pct_abrangente, fill = recorte)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = pct_abrangente), position = position_dodge(width = 0.7),
            vjust = -0.4, size = 2.8) +
  scale_fill_manual(values = c("CNAE 26" = "#c0392b", "Média da indústria" = "#7f8c8d"), name = NULL) +
  facet_wrap(~ tecnologia, ncol = 2, scales = "free_x") +
  labs(
    title = "Grau de utilização (uso abrangente) entre firmas que já adotaram",
    subtitle = "% de firmas adotantes que usam a tecnologia de forma abrangente (não apenas localizada), por função",
    x = NULL, y = "% de uso abrangente (entre adotantes)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

print(grafico_grau_utilizacao)

dir.create(here("output"), showWarnings = FALSE)
ggsave(here("output", "grau_utilizacao_cnae26_vs_media.png"), grafico_grau_utilizacao,
       width = 10, height = 8, dpi = 300)