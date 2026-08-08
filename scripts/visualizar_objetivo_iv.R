library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

if (!exists("objetivo_iv_consolidado")) {
  source(here("scripts", "analise_objetivo_iv.R"))
}

# ---------------------------------------------------------------------
# Reorganizar para o gráfico: uma linha por tecnologia x função, com
# adoção (eixo X) e grau de utilização abrangente (eixo Y)
# ---------------------------------------------------------------------

dados_scatter <- objetivo_iv_consolidado %>%
  transmute(
    tecnologia,
    Administração_adocao = adocao_administracao,
    Administração_abrangente = abrangente_administracao,
    `Desenv. de projetos_adocao` = adocao_projeto,
    `Desenv. de projetos_abrangente` = abrangente_projeto,
    Produção_adocao = adocao_producao,
    Produção_abrangente = abrangente_producao
  ) %>%
  pivot_longer(
    cols = -tecnologia,
    names_to = c("funcao", ".value"),
    names_pattern = "(.+)_(adocao|abrangente)"
  ) %>%
  mutate(funcao = factor(funcao, levels = c("Administração", "Desenv. de projetos", "Produção")))

grafico_objetivo_iv <- ggplot(dados_scatter, aes(x = adocao, y = abrangente, color = funcao)) +
  geom_line(aes(group = tecnologia), color = "grey70", linewidth = 0.4) +
  geom_point(size = 3.2) +
  ggrepel::geom_text_repel(aes(label = funcao), size = 2.7, show.legend = FALSE,
                           seed = 42, min.segment.length = 0.2) +
  scale_color_manual(values = c("Administração" = "#2c3e50",
                                "Desenv. de projetos" = "#c0392b",
                                "Produção" = "#e67e22"), name = NULL) +
  facet_wrap(~ tecnologia, scales = "free", ncol = 2) +
  labs(
    title = "CNAE 26: onde Desenvolvimento de projetos se posiciona",
    subtitle = "Eixo X = % adoção da tecnologia; Eixo Y = % uso abrangente entre adotantes",
    x = "% de firmas que adotaram", y = "% de uso abrangente (entre adotantes)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

print(grafico_objetivo_iv)

dir.create(here("output"), showWarnings = FALSE)
ggsave(here("output", "objetivo_iv_posicionamento_projeto.png"), grafico_objetivo_iv,
       width = 10, height = 9, dpi = 300)