library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)
library(here)

# ---------------------------------------------------------------------
# Rutas del proyecto (usando here() para que siempre encuentre la raíz
# del proyecto sin importar desde qué carpeta se corra el script)
# ---------------------------------------------------------------------

path_cnae <- here("data", "raw", "PINTEC_Semestral_2024_Indicadores_Tematicos_CNAE.xlsx")

#' Acorta los nombres de columna larguísimos que resultan de colapsar todos
#' los niveles de header (função de negócio + grau de utilização). No cambia
#' el contenido, solo hace los nombres manejables para filtrar/graficar.
#' Se puede correr sobre cualquier vector de nombres via make_clean_names.
simplify_pintec_names <- function(nomes) {
  reemplazos <- c(
    "por_grau_de_utilizacao_nas_areas_funcoes_de_negocio_" = "",
    "desenvolvimento_de_projetos_de_produtos_processos_e_servicos_p_d_design_e_engenharia_de_produtos_e_processos" = "projeto",
    "producao_processo_produtivo_manutencao_e_controle_da_qualidade" = "producao",
    "logistica_logistica_de_compras_de_insumos_e_distribuicao_de_produtos_e_servicos" = "logistica",
    "administracao_planejamento_contabilidade_recursos_humanos_financas_e_ti" = "administracao",
    "comercializacao_marketing_vendas_e_pos_vendas" = "comercializacao",
    "_utilizacao_abrangente" = "_abrangente",
    "_utilizacao_localizada" = "_localizada",
    "_utilizou_1" = "_utilizou",
    "_nao_utilizou_2" = "_nao_utilizou",
    "_nao_se_aplica_3" = "_nao_se_aplica"
  )
  str_replace_all(nomes, reemplazos)
}

#' Lee una hoja de las tablas temáticas de PINTEC (Indicadores_Tematicos_CNAE
#' o _PO, y sus versiones de coeficiente de variação).
#'
#' Maneja automáticamente:
#'  - headers multi-fila fusionados (de largo variable según la tabla)
#'  - etiquetas de categoria/CNAE partidas en dos filas por wrap de texto
#'  - nivel jerárquico (Total / grande categoria / divisão CNAE ou PO)
#'
#' @param path ruta al archivo .xlsx
#' @param sheet nombre de la hoja (ej. "tab1.3.1")
#' @param anchor texto que marca el inicio de los datos.
#'   "Total Indústria" para los archivos _CNAE, "Total" para los _PO
#'   (ajustar si tu ancla es otra)
read_pintec_tema <- function(path, sheet, anchor = "Total Indústria") {
  
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
  
  anchor_row <- which(raw[[1]] == anchor)[1]
  if (is.na(anchor_row)) {
    stop("No encontré la fila ancla '", anchor, "' en ", sheet,
         ". Revisá con View(raw) cuál es el texto exacto de la primera fila de datos.")
  }
  
  # la fila 1 es el titulo de la tabla ("Tabela 1.3.1 - ...") y NO debe
  # entrar en la construcción de nombres de columna, o se pega a todos
  title_row <- which(str_starts(coalesce(raw[[1]], ""), "Tabela"))[1]
  if (is.na(title_row)) title_row <- 1
  
  header_rows <- (title_row + 1):(anchor_row - 1)
  
  fonte_row <- which(str_starts(coalesce(raw[[1]], ""), "Fonte"))[1]
  if (is.na(fonte_row)) fonte_row <- nrow(raw) + 1
  data_rows <- anchor_row:(fonte_row - 1)
  
  # --- reconstruir nombres de columna a partir de las filas de header ---
  hdr <- raw[header_rows, ]
  hdr_t <- as_tibble(t(hdr), .name_repair = "minimal")
  names(hdr_t) <- paste0("h", seq_len(ncol(hdr_t)))
  hdr_t <- hdr_t %>% fill(everything(), .direction = "down")
  
  col_names <- apply(hdr_t, 1, function(x) {
    parts <- unique(na.omit(x))
    if (length(parts) == 0) return("na")
    paste(parts, collapse = " | ")
  })
  col_names[1] <- "categoria"
  col_names <- make_clean_names(col_names, allow_dupes = TRUE)
  
  df <- raw[data_rows, ]
  names(df) <- col_names
  
  # --- convertir a numérico todo menos la categoria ---
  df <- df %>% mutate(across(-categoria, ~ suppressWarnings(as.numeric(.x))))
  
  # --- descartar columna(s) fantasma ---
  # El fill(direction="down") sobre las filas de header a veces arrastra una
  # etiqueta (típicamente "Não se aplica (3)") hacia una columna extra al
  # final que en los datos siempre viene vacía. Esa columna queda con un
  # nombre duplicado de una columna real y 100% NA -- la descartamos acá,
  # antes de que cause ambigüedad más adelante.
  df <- df %>% janitor::remove_empty("cols")
  
  # --- reparar etiquetas partidas en dos filas (todas las cols numéricas = NA) ---
  sin_datos <- rowSums(!is.na(select(df, -categoria))) == 0
  keep <- rep(TRUE, nrow(df))
  for (i in seq_len(nrow(df) - 1)) {
    if (isTRUE(sin_datos[i])) {
      df$categoria[i + 1] <- paste(
        str_trim(df$categoria[i], side = "right"),
        str_trim(df$categoria[i + 1], side = "left")
      )
      keep[i] <- FALSE
    }
  }
  df <- df[keep, ]
  
  # --- nivel jerárquico (usa la indentación original antes de limpiar espacios) ---
  df <- df %>%
    mutate(
      nivel = case_when(
        categoria == anchor ~ "total",
        str_starts(categoria, "Indústrias extrativas|Indústrias de transformação|Setor de serviços") ~ "grande_categoria",
        TRUE ~ "categoria_detalhe"
      ),
      categoria = str_squish(categoria)
    ) %>%
    relocate(nivel, .after = categoria)
  
  # --- nombres de columna cortos y legibles ---
  names(df) <- simplify_pintec_names(names(df))
  
  df
}

# ---------------------------------------------------------------------
# Cargar varias hojas de una vez (ya validamos la lógica con tab1.3.1)
# ---------------------------------------------------------------------

hojas_cnae <- c("tab1.2", "tab1.3.1", "tab1.3.2", "tab1.3.3", "tab1.3.4",
                "tab1.3.5", "tab1.3.6", "tab1.4", "tab1.5", "tab1.5_A",
                "tab1.6", "tab1.7")

tablas_cnae <- purrr::map(hojas_cnae, ~ read_pintec_tema(path_cnae, .x)) %>%
  setNames(hojas_cnae)

# ejemplos de uso:
# tablas_cnae[["tab1.3.2"]]           -> accede a una hoja puntual
# nrow(tablas_cnae[["tab1.4"]])       -> chequeo rápido de filas
# names(tablas_cnae[["tab1.5"]])      -> nombres de columna de esa hoja

# chequeo rápido de que todas cargaron con 27 filas (Total + extrativas +
# transformação + 24 divisões) -- si alguna da distinto, hay que revisarla
purrr::map_int(tablas_cnae, nrow)

# ---------------------------------------------------------------------
# Guardar para no tener que reprocesar los Excel cada vez que se abre
# la sesión. Sin redondear -- el redondeo se hace al final, solo para
# lo que vaya al paper/presentación, no acá.
# ---------------------------------------------------------------------

saveRDS(tablas_cnae, here("data", "processed", "tablas_cnae.rds"))

# para volver a cargarlo en una sesión futura, sin correr todo el script:
#   tablas_cnae <- readRDS(here("data", "processed", "tablas_cnae.rds"))

dir.create(here("data", "processed", "tablas_csv"), showWarnings = FALSE)
purrr::iwalk(tablas_cnae, ~ write.csv(.x,
                                      file = here("data", "processed", "tablas_csv", paste0(.y, ".csv")),
                                      row.names = FALSE,
                                      fileEncoding = "UTF-8"))

