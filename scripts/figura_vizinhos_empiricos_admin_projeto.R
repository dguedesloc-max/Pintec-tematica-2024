# =============================================================================
# Figura - CNAE 26 e vizinhos empíricos mais próximos
# Par Administração - Desenvolvimento de projetos, Cloud e IoT
# (mesmo critério de seleção das figuras equivalentes para os outros pares:
#  percentil extremo + CV confiável. Cloud e IoT são os únicos casos sólidos
#  neste par entre as seis tecnologias.)
# =============================================================================

library(dplyr)
library(ggplot2)
library(here)

# -----------------------------------------------------------------------------
# 1. Carregar os vizinhos empíricos (já calculados em visualizar_heterogeneidade.R)
# -----------------------------------------------------------------------------

vizinhos <- read.csv(
  here("data", "processed", "vizinhos_cnae26_admin_projeto.csv"),
  stringsAsFactors = FALSE
) %>%
  filter(tecnologia %in% c("Cloud", "IoT")) %>%
  mutate(
    rotulo = paste0("CNAE ", codigo_cnae),
    tecnologia = factor(tecnologia, levels = c("Cloud", "IoT")),
    aviso_cv = factor(aviso_cv, levels = c("OK", "CV alto", "Sem dado")),
    rotulo_interno = paste(tecnologia, rotulo, sep = "___")
  ) %>%
  group_by(tecnologia) %>%
  mutate(rotulo_interno = factor(rotulo_interno, levels = rotulo_interno[order(posicao_ranking)])) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 2. Gráfico
# -----------------------------------------------------------------------------

cores_preenchimento <- c("TRUE" = "#c0392b", "FALSE" = "#95a5a6")
cores_borda <- c("OK" = "transparent", "CV alto" = "black", "Sem dado" = "black")

grafico_vizinhos_admin_projeto <- ggplot(vizinhos, aes(x = rotulo_interno, y = asimetria_pp)) +
  geom_col(aes(fill = as.character(is_cnae26), color = aviso_cv), linewidth = 1, width = 0.65) +
  geom_text(aes(label = round(asimetria_pp, 1)),
            vjust = ifelse(vizinhos$asimetria_pp >= 0, -0.5, 1.4), size = 4.5) +
  facet_wrap(~ tecnologia, scales = "free", ncol = 2) +
  scale_x_discrete(labels = function(x) sub("^.*___", "", x)) +
  scale_fill_manual(values = cores_preenchimento,
                    labels = c("TRUE" = "CNAE 26", "FALSE" = "Vizinho empírico"),
                    name = NULL) +
  scale_color_manual(values = cores_borda, name = "Confiabilidade") +
  labs(
    title = "CNAE 26 e seus vizinhos empíricos mais próximos",
    subtitle = "Assimetria Administração\u2212Desenvolvimento de projetos (p.p.). Vizinhos = 2 divisões\nacima e abaixo de CNAE 26 no ranking das 24 divisões CNAE da indústria.",
    x = NULL,
    y = "Assimetria Administração\u2212Projeto (p.p.)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 17),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 9.5)
  )

print(grafico_vizinhos_admin_projeto)

# -----------------------------------------------------------------------------
# 3. Salvar
# -----------------------------------------------------------------------------

dir.create(here("output"), showWarnings = FALSE)
ggsave(
  here("output", "figura_vizinhos_empiricos_admin_projeto.png"),
  grafico_vizinhos_admin_projeto,
  width = 9, height = 6, dpi = 300
)