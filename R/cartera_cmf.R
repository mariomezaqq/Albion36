# =============================================================================
# SCRAPER DE CARTERA DE INVERSIONES (holdings) - CMF
#
# Dos mecanismos distintos segun tipo de entidad (confirmado en vivo, no son
# intercambiables):
#
#  - RGFMU (fondos mutuos abiertos): POST a entidad.php?...&pestania=6 (form
#    id "fm"). La tabla viene embebida directo en la respuesta del POST, sin
#    captcha ni segundo request. Reporta MENSUAL.
#
#  - FINRE (fondos de inversion no rescatables, ej. FI Albion): entidad.php
#    con pestania=59 solo MUESTRA el formulario -- el boton "Consultar" abre
#    una ventana via JS apuntando a un endpoint GET separado bajo
#    institucional/inc/inf_financiera/ifrs_xml/ifrs_cartera_<tipo>.php con
#    parametros rut+periodo (aaaamm). No requiere token/cookies. Reporta
#    TRIMESTRAL (solo mar/jun/sep/dic).
#
# Ambos casos devuelven tablas "Circular 1333"-like: fila 1 = titulo de
# categoria, fila 2 = encabezados, filas siguientes = datos, ultima fila
# suele ser un TOTAL a descartar. NO traen nombre de emisor como texto
# (solo RUT Emisor + Nemotecnico) -- excepto FINRE tipo "M" (Metodo de
# Participacion) y "B" (Bienes Raices), que si traen "Nombre del emisor"
# directo (confirmado en vivo para M).
# =============================================================================
suppressMessages({
  library(httr); library(rvest); library(dplyr); library(stringr); library(tibble)
})

TIPOS_CARTERA_RGFMU <- c(N = "Nacional", E = "Extranjera", O = "En opciones",
                          `F` = "En futuros y forward", L = "En opciones lanzador")

TIPOS_CARTERA_FINRE <- c(N = "Nacional", E = "Extranjera", M = "Metodo de Participacion",
                          B = "Bienes Raices", O = "En Opciones", `F` = "En Futuros y Forward")

.FINRE_ENDPOINT <- c(N = "ifrs_cartera_nac.php", E = "ifrs_cartera_ext.php",
                      M = "ifrs_cartera_met_part.php", B = "ifrs_cartera_bie_rai.php",
                      O = "ifrs_cartera_op.php", `F` = "ifrs_cartera_fut_fw.php")

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

#' Busca en una pagina la primera tabla con datos utiles (>= 2 filas)
.encontrar_tabla_cartera <- function(pagina) {
  tablas <- pagina %>% rvest::html_nodes("table")
  for (t in tablas) {
    df <- tryCatch(rvest::html_table(t, fill = TRUE), error = function(e) NULL)
    if (!is.null(df) && nrow(df) >= 2) return(df)
  }
  NULL
}

#' Normaliza nombres de columna para deteccion por patron (case/espacios/saltos)
.normalizar_nombres <- function(x) {
  x <- str_replace_all(as.character(x), "[\n\t\r]+", " ")
  x <- str_replace_all(x, "\\d+(\\.\\d+)+", "")   # quita codigos tipo 6.01.01.00
  x <- str_squish(tolower(x))
  x
}

#' Detecta el indice de columna que matchea un patron regex sobre nombres normalizados
.col_por_patron <- function(nombres_norm, patron) {
  idx <- which(str_detect(nombres_norm, patron))
  if (length(idx) > 0) idx[1] else NA_integer_
}

#' Convierte un string numerico formato chileno ("1.234.567,89") a numeric
.num_cl <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(str_replace_all(trimws(as.character(x)), "\\.", ""), ",", ".")))
}

# ---- RGFMU: fetch ----------------------------------------------------------

#' POST pestania=6 para un fondo RGFMU + tipo + periodo
#' @return data.frame crudo o NULL
obtener_cartera_rgfmu_raw <- function(fondo, mm, aa, tipo, cookies = "") {
  url_cmf <- paste0(
    "https://www.cmfchile.cl/institucional/mercados/entidad.php",
    "?mercado=V&rut=", fondo$run, "&grupo=&tipoentidad=RGFMU",
    if (nchar(fondo$row) > 0) paste0("&row=", fondo$row) else "",
    "&vig=VI&control=svs&pestania=6"
  )
  message("[cartera_rgfmu] === ", fondo$nombre, " tipo=", tipo, " periodo=", mm, "/", aa, " ===")
  resp <- tryCatch(
    httr::POST(url_cmf, httr::add_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "Content-Type" = "application/x-www-form-urlencoded",
      "Origin" = "https://www.cmfchile.cl", "Referer" = url_cmf,
      "Cookie" = if (is.null(cookies)) "" else as.character(cookies)
    ), body = list(mercado = "V", rut = fondo$run, grupo = "", tipoentidad = "RGFMU",
                    row = fondo$row, vig = "VI", control = "svs", pestania = "6",
                    mm = mm, aa = aa, tipo = tipo),
    encode = "form", httr::timeout(30)),
    error = function(e) { message("[cartera_rgfmu] httr error: ", e$message); NULL }
  )
  if (is.null(resp)) return(NULL)
  message("[cartera_rgfmu] HTTP: ", httr::status_code(resp))
  if (httr::status_code(resp) != 200) return(NULL)

  html_str <- httr::content(resp, "text", encoding = "UTF-8")
  pagina <- rvest::read_html(html_str)
  df <- .encontrar_tabla_cartera(pagina)
  if (is.null(df)) { message("[cartera_rgfmu] sin tabla en la respuesta."); return(NULL) }
  message("[cartera_rgfmu] tabla cruda: ", nrow(df), " filas x ", ncol(df), " cols")
  df
}

# ---- FINRE: fetch -----------------------------------------------------------

#' GET al endpoint ifrs_xml correspondiente para un fondo FINRE + tipo + periodo.
#' Sin auth (confirmado en vivo).
#' @return data.frame crudo o NULL
obtener_cartera_finre_raw <- function(fondo, mm, aa, tipo) {
  endpoint <- .FINRE_ENDPOINT[[tipo]]
  if (is.null(endpoint)) { message("[cartera_finre] tipo desconocido: ", tipo); return(NULL) }
  url <- paste0("https://www.cmfchile.cl/institucional/inc/inf_financiera/ifrs_xml/",
                endpoint, "?rut=", fondo$run, "&periodo=", aa, mm)
  message("[cartera_finre] === ", fondo$nombre, " tipo=", tipo, " periodo=", mm, "/", aa, " ===")
  message("[cartera_finre] URL: ", url)
  resp <- tryCatch(
    httr::GET(url, httr::add_headers("User-Agent" = "Mozilla/5.0"), httr::timeout(30)),
    error = function(e) { message("[cartera_finre] httr error: ", e$message); NULL }
  )
  if (is.null(resp)) return(NULL)
  message("[cartera_finre] HTTP: ", httr::status_code(resp))
  if (httr::status_code(resp) != 200) return(NULL)

  html_str <- httr::content(resp, "text", encoding = "UTF-8")
  pagina <- tryCatch(rvest::read_html(html_str), error = function(e) NULL)
  if (is.null(pagina)) { message("[cartera_finre] respuesta no es HTML valido."); return(NULL) }
  df <- .encontrar_tabla_cartera(pagina)
  if (is.null(df)) { message("[cartera_finre] sin tabla en la respuesta."); return(NULL) }
  message("[cartera_finre] tabla cruda: ", nrow(df), " filas x ", ncol(df), " cols")
  df
}

# ---- Limpieza / normalizacion ----------------------------------------------

#' Deteccion de columnas por nombre (no posicion), con fallback a "Nombre del
#' emisor" si la tabla ya lo trae (FINRE tipo M/B). El offset de la fila de
#' headers difiere por tipo de entidad (confirmado con datos reales):
#'   - RGFMU: fila 1 = headers (sin fila de titulo separada)
#'   - FINRE: fila 1 = titulo de categoria (repetido en todas las columnas),
#'            fila 2 = headers reales
#' @param fila_header indice de la fila que contiene los encabezados
.limpiar_cartera_generico <- function(df_raw, fondo_nombre, categoria, periodo_label, fila_header) {
  if (is.null(df_raw) || nrow(df_raw) < fila_header) return(NULL)

  nombres <- .normalizar_nombres(as.character(df_raw[fila_header, ]))
  df_raw <- df_raw[-seq_len(fila_header), , drop = FALSE]
  colnames(df_raw) <- nombres

  # descartar fila(s) de TOTAL / vacias (primera columna vacia o == "total")
  primera <- str_squish(tolower(as.character(df_raw[[1]])))
  df_raw <- df_raw[!is.na(primera) & primera != "" & primera != "total", , drop = FALSE]
  if (nrow(df_raw) == 0) return(tibble())

  idx_nemo   <- .col_por_patron(nombres, "nemot")
  idx_rut    <- .col_por_patron(nombres, "rut.*emisor")
  idx_tipo   <- .col_por_patron(nombres, "tipo inst")
  # "nombre.*emisor" (no "nombre del emisor" literal): la categoria Extranjera
  # (RGFMU) usa el header "Nombre Emisor" (sin "del"), FINRE M/B usa
  # "Nombre del emisor" -- el patron laxo cubre ambos.
  idx_nombre <- .col_por_patron(nombres, "nombre.*emisor")

  # "Valorizacion al cierre" es el nombre estandar (N/E/M/B); en categorias de
  # derivados (opciones/futuros/forward) la columna analoga se llama
  # "valorizacion de mercado". % del activo del fondo no siempre existe para
  # derivados (ej. futuros no lo reportan) -- se deja NA en vez de descartar
  # la categoria entera.
  idx_valor <- .col_por_patron(nombres, "valo.izaci.n.*cierre")
  if (is.na(idx_valor)) idx_valor <- .col_por_patron(nombres, "valo.izaci.n.*mercado")
  idx_pct <- .col_por_patron(nombres, "activo del fondo|activo.*fondo")

  if (is.na(idx_valor)) {
    message("[cartera] no se identifico columna de valorizacion. Headers: ",
            paste(nombres, collapse = " | "))
    return(NULL)
  }
  if (is.na(idx_nemo) && is.na(idx_nombre)) {
    message("[cartera] categoria sin identificacion de instrumento por nombre/nemotecnico ",
            "(probable header de 2 niveles no soportado, ej. opciones/futuros). Headers: ",
            paste(nombres, collapse = " | "))
  }

  tibble(
    fondo = fondo_nombre,
    categoria = categoria,
    nemotecnico = if (!is.na(idx_nemo)) trimws(as.character(df_raw[[idx_nemo]])) else NA_character_,
    rut_emisor = if (!is.na(idx_rut)) trimws(as.character(df_raw[[idx_rut]])) else NA_character_,
    tipo_instrumento = if (!is.na(idx_tipo)) trimws(as.character(df_raw[[idx_tipo]])) else NA_character_,
    nombre_emisor_directo = if (!is.na(idx_nombre)) trimws(as.character(df_raw[[idx_nombre]])) else NA_character_,
    valorizacion_cierre = .num_cl(df_raw[[idx_valor]]),
    pct_activo_fondo = if (!is.na(idx_pct)) .num_cl(df_raw[[idx_pct]]) else NA_real_,
    periodo = periodo_label
  ) %>% filter(!is.na(valorizacion_cierre) | !is.na(pct_activo_fondo))
}

#' @return tibble unificado o NULL
limpiar_cartera_rgfmu <- function(df_raw, fondo_nombre, categoria, periodo_label) {
  .limpiar_cartera_generico(df_raw, fondo_nombre, categoria, periodo_label, fila_header = 1)
}

#' @return tibble unificado o NULL
limpiar_cartera_finre <- function(df_raw, fondo_nombre, categoria, periodo_label) {
  .limpiar_cartera_generico(df_raw, fondo_nombre, categoria, periodo_label, fila_header = 2)
}

# ---- Orquestador por fondo + periodo ----------------------------------------

#' Descarga y consolida TODAS las categorias (tipo) de UN fondo para UN periodo.
#' Dispatcha por fondo$tipoentidad. Nunca devuelve NULL (tibble vacio si todo
#' falla), para que el diff downstream no reviente.
#' @param fondo lista con nombre, run, row, tipoentidad
#' @param periodo list(mm, aa, label)
#' @param cred lista(token, cookies) de cargar_credenciales(), puede ser NULL
#' @return tibble unificado (columnas: fondo, categoria, nemotecnico, rut_emisor,
#'         tipo_instrumento, nombre_emisor_directo, valorizacion_cierre,
#'         pct_activo_fondo, periodo)
obtener_cartera_fondo_periodo <- function(fondo, periodo, cred = NULL) {
  es_finre <- identical(fondo$tipoentidad, "FINRE")
  tipos <- if (es_finre) TIPOS_CARTERA_FINRE else TIPOS_CARTERA_RGFMU
  cookies <- cred$cookies %||% ""

  filas <- lapply(names(tipos), function(codigo) {
    categoria <- tipos[[codigo]]
    df_raw <- if (es_finre) {
      obtener_cartera_finre_raw(fondo, periodo$mm, periodo$aa, codigo)
    } else {
      obtener_cartera_rgfmu_raw(fondo, periodo$mm, periodo$aa, codigo, cookies)
    }
    limpio <- if (es_finre) {
      limpiar_cartera_finre(df_raw, fondo$nombre, categoria, periodo$label)
    } else {
      limpiar_cartera_rgfmu(df_raw, fondo$nombre, categoria, periodo$label)
    }
    if (is.null(limpio)) tibble() else limpio
  })

  bind_rows(filas)
}
