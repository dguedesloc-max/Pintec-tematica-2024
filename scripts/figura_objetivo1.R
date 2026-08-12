# =============================================================================
# Figura de abertura - Objetivo (i): Assimetria funcional na adoção de TDA
# Média da indústria, três funções de negócio, seis tecnologias
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# -----------------------------------------------------------------------------
# 1. Carregar os dados (já calculados em analise_assimetria.R)
# -----------------------------------------------------------------------------

if (!exists("assimetria_larga")) {
  source(here("scripts", "analise_assimetria.R"))
}

dados_long <- assimetria_larga %>%
  filter(recorte == "media_industria") %>%
  select(tecnologia, administracao, producao, projeto) %>%
  pivot_longer(
    cols = c(administracao, producao, projeto),
    names_to = "funcao",
    values_to = "valor"
  ) %>%
  mutate(
    funcao = recode(funcao,
                    "administracao" = "Administração",
                    "projeto"       = "Desenv. de projetos",
                    "producao"      = "Produção"
    ),
    funcao = factor(funcao, levels = c("Administração", "Desenv. de projetos", "Produção")),
    tecnologia = factor(tecnologia, levels = c(
      # Transversais/cognitivas primeiro, físico-operacionais depois
      "IA", "Cloud", "Big Data",
      "IoT", "Robótica", "Manufatura aditiva"
    ))
  )

# -----------------------------------------------------------------------------
# 2. Gráfico
# -----------------------------------------------------------------------------

# mesma paleta por função usada na Figura do objetivo (iv), para consistência
# visual ao longo do paper -- o vermelho fica reservado para CNAE 26 nas
# demais figuras, então aqui NÃO se usa vermelho como cor "genérica"
cores_funcao <- c(
  "Administração" = "#2c3e50",
  "Desenv. de projetos" = "#c0392b",
  "Produção" = "#e67e22"
)

grafico_obj1 <- ggplot(dados_long, aes(x = funcao, y = valor, fill = funcao)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = round(valor, 1)), vjust = -0.4, size = 3.3) +
  facet_wrap(~ tecnologia, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = cores_funcao, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Adoção de TDA por função de negócio",
    subtitle = "% de firmas da indústria que utilizam a tecnologia em cada função (média da indústria)",
    x = NULL,
    y = "% de firmas adotantes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 9, angle = 20, hjust = 1, margin = margin(t = 2)),
    panel.spacing.x = unit(1.5, "lines")
  )

print(grafico_obj1)

# -----------------------------------------------------------------------------
# 3. Salvar
# -----------------------------------------------------------------------------

dir.create(here("output"), showWarnings = FALSE)
ggsave(
  here("output", "figura_objetivo1_assimetria_funcional.png"),
  grafico_obj1,
  width = 8, height = 10, dpi = 300
)