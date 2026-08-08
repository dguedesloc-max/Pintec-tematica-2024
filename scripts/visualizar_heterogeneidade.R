library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(here)

# ggrepel evita que las etiquetas de los setores vecinos se superpongan.
# Si no lo tenés instalado: install.packages("ggrepel")
library(ggrepel)

# ---------------------------------------------------------------------
# Requiere heterogeneidade_setorial (formato ancho: categoria, tecnologia,
# administracao/producao/projeto + sus CV, y los 3 asimetria_*_pp), del
# script analise_heterogeneidade.R. Si no está, se recalcula.
# ---------------------------------------------------------------------

if (!exists("heterogeneidade_setorial")) {
  source(here("scripts", "analise_heterogeneidade.R"))
}

# ---------------------------------------------------------------------
# Mapeo nombre da divisão -> código CNAE (2 dígitos). La planilha de
# PINTEC no trae el código, solo el nombre -- este es el orden estándar
# do IBGE para as 24 divisões de Indústrias de transformação (10 a 33),
# en el mismo orden en que aparecen en las hojas de PINTEC.
# ---------------------------------------------------------------------

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

# ---------------------------------------------------------------------
# Definición de los 3 pares: qué columna de asimetria usar, y qué dos
# columnas de CV determinan la confiabilidade de ese par
# ---------------------------------------------------------------------

pares_def <- list(
  admin_producao = list(
    col = "asimetria_admin_producao_pp",
    cv1 = "cv_administracao", cv2 = "cv_producao",
    titulo = "Assimetria Administração − Produção",
    eixo_x = "Assimetria (p.p.): % Administração − % Produção"
  ),
  admin_projeto = list(
    col = "asimetria_admin_projeto_pp",
    cv1 = "cv_administracao", cv2 = "cv_projeto",
    titulo = "Assimetria Administração − Desenvolvimento de projetos",
    eixo_x = "Assimetria (p.p.): % Administração − % Desenv. de projetos"
  ),
  projeto_producao = list(
    col = "asimetria_projeto_producao_pp",
    cv1 = "cv_projeto", cv2 = "cv_producao",
    titulo = "Assimetria Desenvolvimento de projetos − Produção",
    eixo_x = "Assimetria (p.p.): % Desenv. de projetos − % Produção"
  )
)

# ---------------------------------------------------------------------
# Función que arma la tabla de vecinos + el gráfico para un par dado
# ---------------------------------------------------------------------

construir_grafico_par <- function(par_nome, def) {
  
  dados <- heterogeneidade_setorial %>%
    transmute(
      tecnologia, categoria,
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
      categoria_curta = paste0("CNAE ", codigos_cnae[categoria])
    )
  
  # vecinos: 2 setores arriba y 2 abajo de CNAE 26 en el ranking de este par
  vizinhos <- dados %>%
    group_by(tecnologia) %>%
    arrange(desc(asimetria_pp), .by_group = TRUE) %>%
    mutate(posicao_ranking = row_number()) %>%
    mutate(pos_cnae26 = posicao_ranking[is_cnae26]) %>%
    filter(posicao_ranking >= pos_cnae26 - 2, posicao_ranking <= pos_cnae26 + 2) %>%
    ungroup()
  
  write.csv(
    vizinhos %>%
      mutate(codigo_cnae = codigos_cnae[categoria]) %>%
      select(tecnologia, posicao_ranking, codigo_cnae, categoria, asimetria_pp, aviso_cv, is_cnae26),
    here("data", "processed", paste0("vizinhos_cnae26_", par_nome, ".csv")),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  
  resumo_media <- dados %>%
    group_by(tecnologia) %>%
    summarise(media_asimetria = mean(asimetria_pp), .groups = "drop")
  
  etiquetar <- dados %>% semi_join(vizinhos, by = c("tecnologia", "categoria"))
  
  grafico <- ggplot(dados, aes(x = asimetria_pp, y = 0)) +
    geom_vline(data = resumo_media, aes(xintercept = media_asimetria),
               linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_jitter(aes(color = is_cnae26, shape = aviso_cv, size = is_cnae26),
                height = 0.15, alpha = 0.85) +
    geom_text_repel(
      data = etiquetar, aes(label = categoria_curta),
      size = 2.6, max.overlaps = Inf, seed = 42,
      segment.size = 0.3, min.segment.length = 0
    ) +
    scale_color_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#7f8c8d"),
                       labels = c("TRUE" = "CNAE 26", "FALSE" = "Outras divisões"),
                       name = NULL) +
    scale_shape_manual(values = c("OK" = 16, "CV alto" = 1, "Sem dado" = 4), name = "Confiabilidade") +
    scale_size_manual(values = c("TRUE" = 3.5, "FALSE" = 2), guide = "none") +
    facet_wrap(~ tecnologia, scales = "free_x", ncol = 2) +
    labs(
      title = def$titulo,
      subtitle = "24 divisões CNAE da indústria, por tecnologia. Linha tracejada = média das divisões.",
      x = def$eixo_x,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )
  
  list(grafico = grafico, vizinhos = vizinhos)
}

# ---------------------------------------------------------------------
# Correr para los 3 pares, mostrar cada gráfico en su propio plot, y
# guardar cada uno como PNG separado
# ---------------------------------------------------------------------

dir.create(here("output"), showWarnings = FALSE)

resultados_pares <- imap(pares_def, function(def, par_nome) {
  res <- construir_grafico_par(par_nome, def)
  print(res$grafico)
  ggsave(
    here("output", paste0("heterogeneidade_", par_nome, "_dotplot.png")),
    res$grafico, width = 10, height = 7, dpi = 300
  )
  res
})

# para volver a ver un gráfico puntual sin recorrer todo de nuevo:
# resultados_pares[["projeto_producao"]]$grafico
# resultados_pares[["admin_projeto"]]$vizinhos