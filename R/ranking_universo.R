# =============================================================================
# RANKING YTD -- tabla rica original (reconstruida)
#
# La tabla original de "Albion vs. Universo 36" (nunca estuvo en git, se
# perdio al redesplegar encima sin querer) traia, ademas de YTD: TAC serie %,
# VC, 1D/Semana/MTD/30D/90D/mes-calendario-anterior/YTD %, Vol. Anualizada,
# Patrimonio, Aportantes y Atraso(D), mas tarjetas resumen con el ranking y
# comparacion de Albion contra el promedio del universo.
#
# Se reconstruyo con datos REALES de la CMF (no inventados): la tabla cruda
# de valor cuota (pestania=7) ya trae Patrimonio Neto y Numero de Participes
# -- confirmado en vivo, ver R/scraper.R::limpiar_cmf(). Rentabilidades por
# periodo con el mismo patron que R/rentabilidades_comparar.R (.rent()).
#
# La UNICA columna que no se pudo recuperar de ninguna fuente (ni CMF, ni los
# proyectos hermanos Pulso-VCC/vcc-fondos-dashboard-bundle) es "TAC serie %":
# en el original era una celda editable a mano (doble click), no algo
# scrapeado -- se implementa igual aca como columna editable persistida en
# data/tac_manual.csv, pero arranca vacia hasta que se cargue a mano.
# =============================================================================

RUTA_TAC_MANUAL <- file.path("data", "tac_manual.csv")

.MESES_ABR_ES <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
                    "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")

cargar_tac_manual <- function() {
  if (file.exists(RUTA_TAC_MANUAL)) {
    tryCatch(utils::read.csv(RUTA_TAC_MANUAL, stringsAsFactors = FALSE, colClasses = "character"),
             error = function(e) tibble::tibble(nombre = character(), tac = character()))
  } else tibble::tibble(nombre = character(), tac = character())
}

guardar_tac_manual <- function(tabla) {
  dir.create("data", showWarnings = FALSE)
  utils::write.csv(tabla, RUTA_TAC_MANUAL, row.names = FALSE)
  # Commit a GitHub (ver R/github_store.R) para que la correccion sobreviva a
  # un reinicio del contenedor en shinyapps.io.
  if (exists("store_write_file", mode = "function"))
    tryCatch(store_write_file("tac_manual.csv", RUTA_TAC_MANUAL, mensaje = "Corregir TAC serie %"),
             error = function(e) message("[guardar_tac_manual] FALLO commit a GitHub: ", e$message))
}

#' Dias de atraso: hoy - fecha del ultimo VC disponible en el historico.
.atraso_dias <- function(h, hoy) {
  if (is.null(h) || !nrow(h)) return(NA_integer_)
  as.integer(hoy - max(h$fecha, na.rm = TRUE))
}

#' Volatilidad anualizada: sd(retornos diarios ultimos `ndias`) * sqrt(252).
.vol_anual <- function(h, hoy, ndias = 90) {
  if (is.null(h) || nrow(h) < 6) return(NA_real_)
  hh <- h %>% dplyr::filter(fecha >= hoy - ndias, fecha <= hoy) %>% dplyr::arrange(fecha)
  v <- hh$valor_cuota_ajustado
  v <- v[is.finite(v) & v > 0]
  if (length(v) < 6) return(NA_real_)
  ret <- diff(v) / utils::head(v, -1)
  ret <- ret[is.finite(ret)]
  if (length(ret) < 5) return(NA_real_)
  stats::sd(ret) * sqrt(252)
}

#' Rentabilidad entre los ultimos DOS dias con dato (no un rango de fechas
#' calendario) -- para 1D hace falta el ultimo cierre vs el habil anterior,
#' sin importar fines de semana/feriados en el medio.
.rent_1d <- function(h, hoy) {
  if (is.null(h) || nrow(h) < 2) return(NA_real_)
  ult2 <- h %>% dplyr::filter(fecha <= hoy) %>% dplyr::arrange(dplyr::desc(fecha)) %>% dplyr::slice(1:2)
  if (nrow(ult2) < 2 || ult2$valor_cuota_ajustado[2] <= 0) return(NA_real_)
  ult2$valor_cuota_ajustado[1] / ult2$valor_cuota_ajustado[2] - 1
}

#' Mes calendario anterior completo (ej. si hoy es agosto, julio entero).
.mes_anterior_completo <- function(hoy) {
  primer_dia_mes_actual <- as.Date(format(hoy, "%Y-%m-01"))
  fin <- primer_dia_mes_actual - 1
  ini <- as.Date(format(fin, "%Y-%m-01"))
  list(ini = ini, fin = fin, label = .MESES_ABR_ES[lubridate::month(fin)])
}

#' Fila completa de ranking para UN fondo (todas las columnas de la tabla
#' original). Rango de consulta: ~100 dias atras (alcanza para 90D/Vol) o
#' hasta el 24-dic del año anterior si eso queda mas atras (para YTD).
calcular_fila_ranking <- function(fondo, cred, hoy = Sys.Date() - 1) {
  desde <- min(hoy - 100, as.Date(sprintf("%d-12-24", lubridate::year(hoy) - 1)))
  ref_ytd <- as.Date(sprintf("%d-12-31", lubridate::year(hoy) - 1))
  mes_ant <- .mes_anterior_completo(hoy)

  vacia <- list(nombre = fondo$nombre, serie = fondo$serie, vc = NA_real_,
                d1 = NA_real_, semana = NA_real_, mtd = NA_real_, d30 = NA_real_, d90 = NA_real_,
                mes = NA_real_, mes_label = mes_ant$label, ytd = NA_real_, vol = NA_real_,
                patrimonio = NA_real_, aportantes = NA_real_, atraso = NA_integer_, ok = FALSE)

  h <- tryCatch(scrapear_fondo(fondo, desde, hoy, cred$token, cred$cookies),
                error = function(e) { message("[ranking_universo] ERROR ", fondo$nombre, ": ", e$message); NULL })
  if (is.null(h) || !nrow(h)) return(vacia)
  h <- aplicar_factor_reparto(h)

  ultimo <- h %>% dplyr::filter(fecha <= hoy) %>% dplyr::arrange(dplyr::desc(fecha)) %>% dplyr::slice(1)
  if (!nrow(ultimo)) return(vacia)

  list(
    nombre = fondo$nombre, serie = fondo$serie,
    vc = ultimo$valor_cuota[1],
    d1     = .rent_1d(h, hoy),
    semana = .rent(h, hoy - 7, hoy),
    mtd    = .rent(h, as.Date(format(hoy, "%Y-%m-01")) - 1, hoy),
    d30    = .rent(h, hoy - 30, hoy),
    d90    = .rent(h, hoy - 90, hoy),
    mes    = .rent(h, mes_ant$ini - 1, mes_ant$fin),
    mes_label = mes_ant$label,
    ytd    = .rent(h, ref_ytd, hoy),
    vol    = .vol_anual(h, hoy),
    patrimonio = ultimo$patrimonio[1],
    aportantes = ultimo$aportantes[1],
    atraso = .atraso_dias(h, hoy),
    ok = TRUE
  )
}
