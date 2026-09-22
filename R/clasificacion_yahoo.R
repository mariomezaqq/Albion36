# =============================================================================
# CLASIFICACION RV/RF VIA YAHOO FINANCE (para holdings Extranjeros)
#
# Los holdings Nacionales se clasifican con la heuristica local (tipo
# instrumento + palabras clave, ver R/clasificacion_clase.R). Para los
# holdings Extranjeros (ETFs, fondos internacionales) el nombre que trae la
# CMF a veces no alcanza para saber si son RV o RF (ej. "VanEck Gold Miners
# ETF" no dice "acciones" en ningun lado). Yahoo Finance tiene un buscador
# no oficial (query2.finance.yahoo.com/v1/finance/search) que dado un ticker
# devuelve el nombre largo real + tipo -- de ahi se puede inferir RV/RF con
# las mismas palabras clave, con mejor tasa de acierto que adivinar por el
# nombre truncado de la CMF.
#
# Cache persistente en disco (data/yahoo_cache.rds) porque son llamadas de
# red -- sin cache, cada comparacion volveria a consultar Yahoo por cada
# ticker ya visto.
# =============================================================================
suppressMessages({ library(httr); library(jsonlite); library(stringr) })

RUTA_YAHOO_CACHE <- file.path("data", "yahoo_cache.rds")

cargar_yahoo_cache <- function() {
  if (file.exists(RUTA_YAHOO_CACHE)) tryCatch(readRDS(RUTA_YAHOO_CACHE), error = function(e) list()) else list()
}

guardar_yahoo_cache <- function(cache) {
  dir.create("data", showWarnings = FALSE)
  saveRDS(cache, RUTA_YAHOO_CACHE)
}

#' Busca un ticker/ISIN en el buscador de Yahoo Finance.
#' @return list(longname, quoteType) o NULL si no encontro nada / fallo la red
buscar_yahoo <- function(simbolo) {
  url <- paste0("https://query2.finance.yahoo.com/v1/finance/search?q=", utils::URLencode(simbolo, reserved = TRUE))
  resp <- tryCatch(httr::GET(url, httr::add_headers("User-Agent" = "Mozilla/5.0"), httr::timeout(8)),
                    error = function(e) { message("[yahoo] error de red para '", simbolo, "': ", e$message); NULL })
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  js <- tryCatch(jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(js) || is.null(js$quotes) || !is.data.frame(js$quotes) || nrow(js$quotes) == 0) return(NULL)
  q <- js$quotes[1, ]
  longname <- if (!is.null(q$longname) && !is.na(q$longname) && nzchar(q$longname)) q$longname else q$shortname
  list(longname = longname, quoteType = if (!is.null(q$quoteType)) q$quoteType else NA_character_)
}

.primer_token <- function(x) trimws(str_split(trimws(x), "\\s+", simplify = TRUE)[1])

#' Busca info Yahoo para un holding Extranjero probando, en orden, hasta dar
#' con un resultado (confirmado empiricamente con datos reales de carteras --
#' ver scratchpad de la sesion que agrego esto):
#'   1. el nemotecnico tal cual (por si ya es un ticker valido, ej. "GDX")
#'   2. el primer token del nemotecnico (los holdings tipo ETFA/CFME de la
#'      CMF traen el nemotecnico en formato Bloomberg "TICKER EXCH EQUITY",
#'      ej. "EIMI IE EQUITY" -- el primer token SI es un ticker real de Yahoo
#'      para ETFs UCITS listados)
#'   3. el primer token + ".L" (Bolsa de Londres -- resuelve casos donde el
#'      ticker bare choca con otro instrumento no relacionado en Yahoo, ej.
#'      "SPXS" bare trae un ETF apalancado de EEUU, "SPXS.L" trae el UCITS
#'      correcto)
#'   4. el nombre truncado que trae la CMF como texto libre de busqueda --
#'      para fondos CFME (cuotas de fondos SICAV/OEIC no listados en bolsa,
#'      sin ticker publico) el nemotecnico Bloomberg nunca matchea nada en
#'      Yahoo, pero buscar el nombre SI encuentra el fondo real pese a venir
#'      truncado (ej. "BLACKROCK GLOBAL FUNDS - FIXED" -> "BGF Fixed Income
#'      Global Opps I2")
#' Cachea el resultado final (o "sin match") bajo el nemotecnico para no
#' repetir hasta 4 llamadas de red por holding ya visto.
.resolver_yahoo_extranjero <- function(nemotecnico, nombre_cmf, cache) {
  clave <- if (!is.na(nemotecnico) && nzchar(nemotecnico)) nemotecnico else nombre_cmf
  if (is.na(clave) || !nzchar(clave)) return(list(info = NULL, cache = cache))
  if (!is.null(cache[[clave]])) return(list(info = cache[[clave]], cache = cache))

  intentos <- character(0)
  if (!is.na(nemotecnico) && nzchar(nemotecnico)) {
    tok <- .primer_token(nemotecnico)
    intentos <- unique(c(nemotecnico, tok, paste0(tok, ".L")))
  }
  if (!is.na(nombre_cmf) && nzchar(nombre_cmf)) intentos <- c(intentos, nombre_cmf)

  info <- NULL
  for (q in intentos) {
    info <- buscar_yahoo(q)
    if (!is.null(info)) { message("[yahoo] '", clave, "' resuelto via '", q, "' -> ", info$longname); break }
  }
  if (is.null(info)) { message("[yahoo] '", clave, "' -> sin match en ningun intento"); info <- list(longname = NA_character_, quoteType = NA_character_) }
  cache[[clave]] <- info
  list(info = info, cache = cache)
}

#' Clasifica RV/RF/Alternativos de UN holding Extranjero via Yahoo, con cache
#' persistente. Prueba el nemotecnico (como ticker, en varias formas) y si
#' no encuentra nada prueba el nombre truncado de la CMF -- ver
#' .resolver_yahoo_extranjero() para el detalle de cada intento.
#' @param nombre_cmf nombre_emisor tal como lo resolvio la CMF (puede venir
#'   truncado a ~31 caracteres) -- se usa solo como fallback de busqueda
#' @param cache lista en memoria (de cargar_yahoo_cache()), se modifica y se devuelve
#' @return list(rv_rf, cache)
clasificar_yahoo_cacheado <- function(simbolo, nombre_cmf = NA_character_, cache) {
  r <- .resolver_yahoo_extranjero(simbolo, nombre_cmf, cache)
  info <- r$info
  cache <- r$cache
  if (is.null(info) || is.na(info$longname)) return(list(rv_rf = NA_character_, cache = cache))

  nom <- .quitar_tildes(info$longname)
  rv_rf <- dplyr::case_when(
    isTRUE(any(str_detect(nom, .PALABRAS_ALTERNATIVOS), na.rm = TRUE)) ~ "Alternativos",
    isTRUE(any(str_detect(nom, .PALABRAS_RF), na.rm = TRUE)) ~ "RF",
    isTRUE(any(str_detect(nom, .PALABRAS_RV), na.rm = TRUE)) ~ "RV",
    # fallback debil: si Yahoo dice que es un ETF/fondo y el nombre no trajo
    # ninguna palabra clave de deuda, la mayoria observada son accionarios
    # (indices, sectores, paises) -- se marca igual, queda auditable via cache.
    !is.na(info$quoteType) && info$quoteType %in% c("ETF", "MUTUALFUND", "EQUITY") ~ "RV",
    TRUE ~ NA_character_
  )
  list(rv_rf = rv_rf, cache = cache)
}
