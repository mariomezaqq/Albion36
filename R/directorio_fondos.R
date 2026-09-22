# =============================================================================
# DIRECTORIO DE FONDOS (resolucion de nombre de emisor para CFM/CFI)
#
# La cartera de un fondo trae "RUT Emisor" + "Nemotecnico" pero NO el nombre
# legible cuando el instrumento es una cuota de OTRO fondo (tipo_instrumento
# CFM = fondo mutuo, CFI = fondo de inversion). La CMF publica un listado
# masivo de codigos->nombre para cada caso; lo scrapeamos una vez y lo
# cacheamos localmente (son tablas de referencia compartidas, no por-fondo).
#
# Estructura confirmada en vivo (idem para ambas paginas): bloques repetidos
# de "Rut/Dv/Razon Social Administradora" seguidos de filas
# "Run/Dv/Razon Social Fondo/Nombre Fondo/Serie/Nemotecnico".
# =============================================================================
suppressMessages({ library(httr); library(rvest); library(dplyr); library(stringr); library(tibble) })

RUTA_DIR_CFM <- file.path("data", "directorio_cfm.rds")
RUTA_DIR_CFI <- file.path("data", "directorio_cfi.rds")

.URL_DIR_CFM <- "https://www.cmfchile.cl/institucional/seil/certificacion_cir1835_fmutuos.php"
.URL_DIR_CFI <- "https://www.cmfchile.cl/institucional/seil/certificacion_cir1835_finversion.php"

#' Scrapea uno de los listados masivos y devuelve tibble(nemotecnico, nombre_fondo,
#' razon_social_fondo, serie). Descarta las filas de encabezado de bloque
#' (administradora / "Run Dv Razon Social Fondo...") que quedan mezcladas
#' como filas normales en la tabla.
.scrapear_directorio <- function(url, etiqueta) {
  message("[directorio_", etiqueta, "] GET ", url)
  resp <- tryCatch(httr::GET(url, httr::add_headers("User-Agent" = "Mozilla/5.0"), httr::timeout(60)),
                    error = function(e) { message("[directorio_", etiqueta, "] httr error: ", e$message); NULL })
  if (is.null(resp)) return(NULL)
  message("[directorio_", etiqueta, "] HTTP: ", httr::status_code(resp))
  if (httr::status_code(resp) != 200) return(NULL)

  pagina <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))
  tablas <- pagina %>% rvest::html_nodes("table")
  if (length(tablas) == 0) { message("[directorio_", etiqueta, "] sin tabla en la respuesta."); return(NULL) }
  df <- rvest::html_table(tablas[[1]], fill = TRUE)
  message("[directorio_", etiqueta, "] tabla cruda: ", nrow(df), " filas")

  # Columnas (por posicion, layout fijo confirmado en vivo): Run, Dv, Razon
  # Social Fondo, Nombre Fondo, Serie, Nemotecnico. Las filas de encabezado
  # de bloque repiten literalmente "Run"/"Rut" en la primera columna -> se
  # descartan junto con filas de administradora (Run no numerico).
  colnames(df) <- c("run", "dv", "razon_social_fondo", "nombre_fondo", "serie", "nemotecnico")[seq_len(ncol(df))]
  out <- df %>%
    filter(str_detect(trimws(as.character(run)), "^\\d+$")) %>%
    transmute(
      run = trimws(as.character(run)),
      dv = trimws(as.character(dv)),
      nemotecnico = trimws(as.character(nemotecnico)),
      nombre_fondo = trimws(as.character(nombre_fondo)),
      razon_social_fondo = trimws(as.character(razon_social_fondo)),
      serie = trimws(as.character(serie))
    ) %>%
    filter(nemotecnico != "") %>%
    distinct(nemotecnico, .keep_all = TRUE)

  message("[directorio_", etiqueta, "] filas utiles: ", nrow(out))
  out
}

#' Directorio CFM (fondos mutuos), con cache en disco. @param forzar re-descarga aunque exista cache
obtener_directorio_cfm <- function(forzar = FALSE) {
  if (!forzar && file.exists(RUTA_DIR_CFM)) return(tryCatch(readRDS(RUTA_DIR_CFM), error = function(e) NULL))
  dir <- .scrapear_directorio(.URL_DIR_CFM, "cfm")
  if (!is.null(dir)) { dir.create("data", showWarnings = FALSE); saveRDS(dir, RUTA_DIR_CFM) }
  dir
}

#' Directorio CFI (fondos de inversion), con cache en disco.
obtener_directorio_cfi <- function(forzar = FALSE) {
  if (!forzar && file.exists(RUTA_DIR_CFI)) return(tryCatch(readRDS(RUTA_DIR_CFI), error = function(e) NULL))
  dir <- .scrapear_directorio(.URL_DIR_CFI, "cfi")
  if (!is.null(dir)) { dir.create("data", showWarnings = FALSE); saveRDS(dir, RUTA_DIR_CFI) }
  dir
}

#' Agrega/completa la columna nombre_emisor a una tabla de cartera ya unificada.
#' Regla: tipo_instrumento == "CFM" -> cruza contra dir_cfm por nemotecnico
#'        tipo_instrumento == "CFI" -> cruza contra dir_cfi por nemotecnico
#'        ya trae nombre_emisor_directo (FINRE M/B)          -> se usa tal cual
#'        cualquier otro caso                                -> fallback nemotecnico, luego rut_emisor
#' @param tabla_cartera tibble con columnas nemotecnico, tipo_instrumento, nombre_emisor_directo, rut_emisor
#' @param dir_cfm,dir_cfi tibbles de obtener_directorio_cfm()/obtener_directorio_cfi(), o NULL (se cargan de cache)
#' @return tabla_cartera + columna nombre_emisor
resolver_nombre_emisor <- function(tabla_cartera, dir_cfm = NULL, dir_cfi = NULL) {
  if (is.null(tabla_cartera) || nrow(tabla_cartera) == 0) {
    if (!is.null(tabla_cartera)) tabla_cartera$nombre_emisor <- character(0)
    return(tabla_cartera)
  }
  if (is.null(dir_cfm)) dir_cfm <- obtener_directorio_cfm()
  if (is.null(dir_cfi)) dir_cfi <- obtener_directorio_cfi()

  # razon_social_fondo (nombre oficial completo, ej. "FONDO MUTUO ITAU CARTERA
  # DINAMICO PLUS") en vez de nombre_fondo (nombre corto, ej. "DINAMICO PLUS")
  # -- el nombre corto no alcanza para saber a que familia de fondos pertenece.
  nombre_cfm <- if (!is.null(dir_cfm)) dir_cfm$razon_social_fondo[match(tabla_cartera$nemotecnico, dir_cfm$nemotecnico)] else NA_character_
  nombre_cfi <- if (!is.null(dir_cfi)) dir_cfi$razon_social_fondo[match(tabla_cartera$nemotecnico, dir_cfi$nemotecnico)] else NA_character_

  tabla_cartera %>% mutate(
    nombre_emisor = dplyr::case_when(
      !is.na(.data$nombre_emisor_directo) & .data$nombre_emisor_directo != "" ~ .data$nombre_emisor_directo,
      .data$tipo_instrumento == "CFM" & !is.na(nombre_cfm) ~ nombre_cfm,
      .data$tipo_instrumento == "CFI" & !is.na(nombre_cfi) ~ nombre_cfi,
      !is.na(.data$nemotecnico) & .data$nemotecnico != "" ~ .data$nemotecnico,
      TRUE ~ .data$rut_emisor
    )
  )
}
