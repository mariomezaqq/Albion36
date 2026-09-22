# =============================================================================
# CLASIFICACION "CLASE" (RV/RF x Nacional/Internacional)
#
# No hay ninguna fuente CMF que traiga esto directo -- se arma con una
# heuristica en capas, de mas a menos confiable:
#   1. Override manual por nemotecnico (data/clase_overrides.csv) -- para
#      corregir a mano los casos que la heuristica clasifique mal o no
#      pueda resolver. Este archivo es el mecanismo pensado para ir
#      afinando la clasificacion con el tiempo sin tocar codigo: basta
#      agregar una fila "nemotecnico,clase".
#   2. Por Tipo Instrumento, con los codigos que indico el usuario (ACC =
#      renta variable, BB = renta fija) mas los codigos habituales de la
#      CMF para instrumentos de deuda (bonos, pagares, depositos).
#   3. Por palabras clave en el nombre resuelto del emisor/instrumento
#      (ej. "DEUDA", "BONO", "ACCIONES", "EQUITY").
#   4. Si nada matchea: NA (Sin Clasificar) -- a proposito no se fuerza una
#      clase sin evidencia; mejor visible como pendiente que silenciosamente
#      mal clasificado.
#
# El eje Nacional/Internacional sale directo de la columna `categoria` del
# holding (Nacional -> Nacional, Extranjera -> Internacional). Categorias
# que no son ninguna de esas dos (Opciones, Futuros, Metodo de Participacion,
# Bienes Raices) no traen ese dato en la tabla unificada actual -> quedan
# "Sin Clasificar" tambien, no se adivina.
# =============================================================================
suppressMessages({ library(dplyr); library(stringr) })

RUTA_CLASE_OVERRIDES <- file.path("data", "clase_overrides.csv")

# Tipo Instrumento -> RV/RF (codigos CMF; "ACC"/"BB" son los que dio el
# usuario). "ETFB" NO es code fiable para RF -- el usuario confirmo que hay
# ETFB de RV tambien, asi que esos quedan a criterio de las palabras clave
# sobre el nombre (ver mas abajo) en vez de asumirse por el codigo. El resto
# son los codigos habituales para deuda -- a confirmar/ampliar cuando
# aparezcan en carteras reales de otros fondos.
.TIPO_RV <- c("ACC")
.TIPO_RF <- c("BB", "BE", "BS", "BC", "PS", "PF", "DPF", "DPP", "LH", "PDBC", "PRC", "PAF")

# Palabras clave sobre el nombre resuelto del emisor/instrumento. Se revisan
# en este orden: Alternativos primero (ej. "DEUDA PRIVADA" contiene "DEUDA" y
# si no se chequeara antes caeria mal en RF), despues RF, despues RV.
.PALABRAS_ALTERNATIVOS <- c("DEUDA PRIVADA", "PRIVATE DEBT", "PRIVATE EQUITY",
                             "CAPITAL PRIVADO", "INMOBILIARI", "REAL ESTATE")
.PALABRAS_RF <- c("DEUDA", "BONO", "BONOS", "RENTA FIJA", "LIQUIDEZ", "MONEY MARKET",
                   "AHORRO", "FIXED INCOME", "BOND", "CREDIT", "CREDITO", "CORPORATE",
                   "TESORERIA", "TREASURY", "GOVT", "GOVERNMENT", "CORTO PLAZO", "DEBT", "MBS")
.PALABRAS_RV <- c("ACCION", "ACCIONES", "EQUITY", "RENTA VARIABLE", "ACCIONARIO",
                   "S&P", "MSCI", "FTSE", "STOXX", "RUSSELL", "NASDAQ", "DOW JONES")

# Nemotecnicos puntuales que la heuristica por codigo/palabra no resuelve
# (ej. ETFs de renta fija cuyo nombre no trae "bond"/"deuda"). DNCA lo dio
# el usuario como ejemplo real. Agregar aca los que se detecten a mano, o
# preferir data/clase_overrides.csv si ya se esta usando ese mecanismo.
.NEMOTECNICOS_RF_CONOCIDOS <- c("DNCA")

.quitar_tildes <- function(x) iconv(toupper(x), from = "UTF-8", to = "ASCII//TRANSLIT")

#' Carga el override manual (data/clase_overrides.csv, columnas nemotecnico,clase).
#' No falla si el archivo no existe -- devuelve NULL.
cargar_clase_overrides <- function() {
  if (!file.exists(RUTA_CLASE_OVERRIDES)) return(NULL)
  tryCatch({
    df <- utils::read.csv(RUTA_CLASE_OVERRIDES, stringsAsFactors = FALSE, colClasses = "character")
    if (!all(c("nemotecnico", "clase") %in% names(df))) return(NULL)
    df
  }, error = function(e) NULL)
}

#' Clasifica RV / RF / Alternativos para UN instrumento.
#' @return "RV", "RF", "Alternativos" o NA_character_ si no hay evidencia suficiente
.clasificar_rv_rf <- function(tipo_instrumento, nombre) {
  tipo <- toupper(trimws(tipo_instrumento %||% ""))
  nom <- .quitar_tildes(nombre %||% "")

  # Alternativos primero: "DEUDA PRIVADA" contiene "DEUDA" y caeria en RF si
  # no se revisa antes (dato del usuario: deuda privada = alternativos, no RF).
  # any(..., na.rm=TRUE): si `nom` es NA (filas sin identificacion de
  # instrumento, ej. Opciones/Futuros con header de 2 niveles), str_detect
  # devuelve NA y any() sin na.rm propaga NA -> if(NA) rompe con "missing
  # value where TRUE/FALSE is needed". Con na.rm=TRUE simplemente no matchea.
  if (isTRUE(any(str_detect(nom, .PALABRAS_ALTERNATIVOS), na.rm = TRUE))) return("Alternativos")
  if (tipo %in% .TIPO_RV) return("RV")
  if (tipo %in% .TIPO_RF) return("RF")
  if (isTRUE(any(str_detect(nom, .PALABRAS_RF), na.rm = TRUE))) return("RF")
  if (isTRUE(any(str_detect(nom, .PALABRAS_RV), na.rm = TRUE))) return("RV")
  NA_character_
}

#' Agrega la columna `clase` a una tabla de cartera unificada (una fila por
#' instrumento). Usa nombre_emisor si esta resuelto, si no nombre_emisor_directo,
#' si no nemotecnico -- lo que haya disponible para matchear palabras clave.
#' @param tabla tibble con columnas categoria, tipo_instrumento, nemotecnico,
#'   y alguna de nombre_emisor/nombre_emisor_directo
#' @param overrides tibble de cargar_clase_overrides(), o NULL (se carga si falta)
#' @return tabla + columna clase (valores: "RV Nacional","RV Internacional",
#'   "RF Nacional","RF Internacional","Alternativos Nacional",
#'   "Alternativos Internacional","Sin Clasificar")
clasificar_clase <- function(tabla, overrides = NULL) {
  if (is.null(tabla) || nrow(tabla) == 0) {
    if (!is.null(tabla)) tabla$clase <- character(0)
    return(tabla)
  }
  if (is.null(overrides)) overrides <- cargar_clase_overrides()

  nombre_ref <- if ("nombre_emisor" %in% names(tabla)) tabla$nombre_emisor
                else if ("nombre_emisor_directo" %in% names(tabla)) tabla$nombre_emisor_directo
                else tabla$nemotecnico

  rv_rf <- mapply(.clasificar_rv_rf, tabla$tipo_instrumento, nombre_ref)
  rv_rf[tabla$nemotecnico %in% .NEMOTECNICOS_RF_CONOCIDOS] <- "RF"

  if (!is.null(overrides) && nrow(overrides) > 0) {
    idx <- match(tabla$nemotecnico, overrides$nemotecnico)
    hay_override <- !is.na(idx)
    if (isTRUE(any(hay_override))) {
      # el override ya trae la clase completa (ej. "RF Internacional"), no solo RV/RF
      tabla$clase <- NA_character_
      tabla$clase[hay_override] <- overrides$clase[idx[hay_override]]
    }
  }
  if (!"clase" %in% names(tabla)) tabla$clase <- NA_character_

  nac_int <- dplyr::case_when(
    tabla$categoria == "Nacional" ~ "Nacional",
    tabla$categoria == "Extranjera" ~ "Internacional",
    TRUE ~ NA_character_
  )

  # Para holdings Extranjeros que la heuristica local no pudo resolver, se
  # intenta via Yahoo Finance (nombre real del ticker/ISIN) -- ver
  # R/clasificacion_yahoo.R. Con cache persistente en disco, solo pega a la
  # red la primera vez que se ve cada ticker.
  falta_rv_rf_ext <- is.na(rv_rf) & tabla$categoria == "Extranjera"
  if (isTRUE(any(falta_rv_rf_ext, na.rm = TRUE)) && exists("clasificar_yahoo_cacheado")) {
    yahoo_cache <- cargar_yahoo_cache()
    idx_faltantes <- which(falta_rv_rf_ext)
    for (i in idx_faltantes) {
      r <- clasificar_yahoo_cacheado(tabla$nemotecnico[i], nombre_ref[i], yahoo_cache)
      rv_rf[i] <- r$rv_rf
      yahoo_cache <- r$cache
    }
    guardar_yahoo_cache(yahoo_cache)
  }

  faltantes <- is.na(tabla$clase)
  tabla$clase[faltantes] <- ifelse(
    is.na(rv_rf[faltantes]) | is.na(nac_int[faltantes]),
    "Sin Clasificar",
    paste(rv_rf[faltantes], nac_int[faltantes])
  )
  tabla
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
