library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

if (!exists("grau_utilizacao_larga")) {
  source(here("scripts", "analise_grau_utilizacao.R"))
}

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
            vjust = -0.4, size = 4.5) +
  scale_fill_manual(values = c("CNAE 26" = "#c0392b", "Média da indústria" = "#7f8c8d"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(~ tecnologia, ncol = 2, scales = "free_x") +
  labs(
    title = "Grau de utilização (uso abrangente) entre firmas que já adotaram",
    subtitle = "% de firmas adotantes que usam a tecnologia de forma abrangente (não apenas localizada), por função",
    x = NULL, y = "% de uso abrangente (entre adotantes)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 17),
    axis.text.x = element_text(face = "bold", size = 9.5),
    panel.spacing.x = unit(1.3, "lines"),
    panel.spacing.y = unit(3, "lines")
  )

print(grafico_grau_utilizacao)

dir.create(here("output"), showWarnings = FALSE)
ggsave(here("output", "grau_utilizacao_cnae26_vs_media.png"), grafico_grau_utilizacao,
       width = 11, height = 10, dpi = 300)