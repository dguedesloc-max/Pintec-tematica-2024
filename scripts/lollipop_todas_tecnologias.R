library(dplyr)
library(stringr)
library(ggplot2)
library(here)

if (!exists("heterogeneidade_setorial")) {
  source(here("scripts", "analise_heterogeneidade.R"))
}

codigos_cnae <- c(
  "Fabricação de produtos alimentícios" = "10",
  "Fabricação de bebidas" = "11",
  "Fabricação de produtos do fumo" = "12",
  "Fabricação de produtos têxteis" = "13",
  "Confecção de artigos do vestuário e acessórios" = "14",
  "Preparação de couros e fabricação de artefatos de couro, artigos de viagem e calçados" = "15",
  "Fabricação de produtos de madeira" = "16",
  "Fabricação de celulose, papel e produtos de papel" = "17",
  "Impressão e reprodução de gravações" = "18",
  "Fabricação de coque, de produtos derivados do petróleo e de biocombustíveis" = "19",
  "Fabricação de produtos químicos" = "20",
  "Fabricação de produtos farmoquímicos e farmacêuticos" = "21",
  "Fabricação de artigos de borracha e plástico" = "22",
  "Fabricação de produtos de minerais não-metálicos" = "23",
  "Metalurgia" = "24",
  "Fabricação de produtos de metal" = "25",
  "Fabricação de equipamentos de informática, produtos eletrônicos e ópticos" = "26",
  "Fabricação de máquinas, aparelhos e materiais elétricos" = "27",
  "Fabricação de máquinas e equipamentos" = "28",
  "Fabricação de veículos automotores, reboques e carrocerias" = "29",
  "Fabricação de outros equipamentos de transporte" = "30",
  "Fabricação de móveis" = "31",
  "Fabricação de produtos diversos" = "32",
  "Manutenção, reparação e instalação de máquinas e equipamentos" = "33"
)

pares_def <- list(
  admin_producao = list(
    col = "asimetria_admin_producao_pp",
    cv1 = "cv_administracao", cv2 = "cv_producao",
    titulo_par = "Administração \u2212 Produção"
  ),
  admin_projeto = list(
    col = "asimetria_admin_projeto_pp",
    cv1 = "cv_administracao", cv2 = "cv_projeto",
    titulo_par = "Administração \u2212 Desenvolvimento de projetos"
  ),
  projeto_producao = list(
    col = "asimetria_projeto_producao_pp",
    cv1 = "cv_projeto", cv2 = "cv_producao",
    titulo_par = "Desenvolvimento de projetos \u2212 Produção"
  )
)

#' Gráfico de lollipop horizontal para UNA tecnologia y UN par de funções.
#' Eje Y = las 24 divisões (código CNAE), ordenadas por asimetria.
grafico_lollipop <- function(tecnologia_nome, par_nome) {
  
  def <- pares_def[[par_nome]]
  
  dados <- heterogeneidade_setorial %>%
    filter(tecnologia == tecnologia_nome) %>%
    transmute(
      categoria,
      codigo = paste0("CNAE ", codigos_cnae[categoria]),
      asimetria_pp = .data[[def$col]],
      cv_a = .data[[def$cv1]],
      cv_b = .data[[def$cv2]]
    ) %>%
    mutate(
      aviso_cv = case_when(
        is.na(cv_a) | is.na(cv_b) ~ "Sem dado",
        cv_a > 25 | cv_b > 25 ~ "CV alto",
        TRUE ~ "OK"
      ),
      is_cnae26 = str_detect(categoria, "informática"),
      codigo = factor(codigo, levels = codigo[order(asimetria_pp)])
    )
  
  media <- mean(dados$asimetria_pp)
  
  ggplot(dados, aes(x = asimetria_pp, y = codigo)) +
    geom_vline(xintercept = media, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = asimetria_pp, y = codigo, yend = codigo, color = is_cnae26),
                 linewidth = 0.9) +
    geom_point(aes(color = is_cnae26, shape = aviso_cv), size = 3) +
    scale_color_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#7f8c8d"),
                       labels = c("TRUE" = "CNAE 26", "FALSE" = "Outras divisões"), name = NULL) +
    scale_shape_manual(values = c("OK" = 16, "CV alto" = 1, "Sem dado" = 4), name = "Confiabilidade") +
    guides(shape = guide_legend(override.aes = list(color = "#7f8c8d"))) +
    labs(
      title = paste0(tecnologia_nome, ": ", def$titulo_par),
      subtitle = "24 divisões CNAE da indústria. Linha tracejada = média das divisões.",
      x = "Assimetria (p.p.)", y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
}

# ---------------------------------------------------------------------
# Las 6 tecnologías, los 2 pares centrales (admin_producao y
# projeto_producao) para los objetivos (iii) y (iv). Se guardan todos
# los PNG; se muestran en el panel de Plots solo los de IA para no
# saturar -- navegá con las flechas "Previous/Next Plot" para ver el
# resto, o abrí directo los archivos en output/.
# ---------------------------------------------------------------------

dir.create(here("output"), showWarnings = FALSE)

tecnologias <- c("Big Data", "Cloud", "IA", "IoT", "Manufatura aditiva", "Robótica")
pares_a_graficar <- c("admin_producao", "projeto_producao")

graficos_lollipop <- list()

for (tec in tecnologias) {
  for (par in pares_a_graficar) {
    g <- grafico_lollipop(tec, par)
    print(g)
    nome_arquivo <- paste0("lollipop_", gsub(" ", "_", tec), "_", par, ".png")
    ggsave(here("output", nome_arquivo), g, width = 7, height = 7, dpi = 300)
    graficos_lollipop[[paste(tec, par, sep = "_")]] <- g
  }
}

# para volver a ver un gráfico puntual sin recorrer todo de nuevo:
# graficos_lollipop[["Robótica_projeto_producao"]]
