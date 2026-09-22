# =============================================================================
# PERIODOS DE CARTERA
# RGFMU (fondos mutuos abiertos) publica cartera MENSUAL.
# FINRE (fondos de inversion no rescatables, ej. Albion) publica TRIMESTRAL
# (solo cierres de marzo/junio/septiembre/diciembre).
# =============================================================================
suppressMessages({ library(lubridate) })

#' Ultimo mes calendario cerrado antes de `hoy`
.ultimo_mes_cerrado <- function(hoy) {
  d <- lubridate::floor_date(as.Date(hoy), "month") - lubridate::days(1)
  list(mm = format(d, "%m"), aa = format(d, "%Y"), label = format(d, "%m/%Y"))
}

#' Ultimo cierre trimestral (mar/jun/sep/dic) <= hoy
.ultimo_trimestre_cerrado <- function(hoy) {
  hoy <- as.Date(hoy)
  anio <- lubridate::year(hoy)
  cierres <- as.Date(sprintf(c("%d-03-31", "%d-06-30", "%d-09-30", "%d-12-31"),
                             rep(c(anio - 1, anio), each = 4)))
  d <- max(cierres[cierres <= hoy])
  list(mm = format(d, "%m"), aa = format(d, "%Y"),
       label = paste0(match(format(d, "%m"), c("03", "06", "09", "12")), "T ", format(d, "%Y")))
}

#' Retrocede un periodo (1 mes para RGFMU, 1 trimestre/3 meses para FINRE)
#' Aritmetica de meses enteros -- evita casos borde de fin de mes/anio con Date.
#' @param periodo list(mm, aa, label)
#' @param tipoentidad "RGFMU" o "FINRE"
retroceder_periodo <- function(periodo, tipoentidad) {
  paso <- if (identical(tipoentidad, "FINRE")) 3L else 1L
  meses_desde_origen <- as.integer(periodo$aa) * 12L + (as.integer(periodo$mm) - 1L) - paso
  aa_nuevo <- meses_desde_origen %/% 12L
  mm_nuevo <- sprintf("%02d", meses_desde_origen %% 12L + 1L)
  label <- if (identical(tipoentidad, "FINRE"))
    paste0(match(mm_nuevo, c("03", "06", "09", "12")), "T ", aa_nuevo)
  else
    paste0(mm_nuevo, "/", aa_nuevo)
  list(mm = mm_nuevo, aa = as.character(aa_nuevo), label = label)
}

#' Calcula los dos periodos mas recientes (actual + anterior) segun tipo de entidad
#' @param tipoentidad "RGFMU" (mensual) o "FINRE" (trimestral)
#' @param hoy Date de referencia (default Sys.Date())
#' @return list(actual = list(mm,aa,label), anterior = list(mm,aa,label))
calcular_periodos_recientes <- function(tipoentidad, hoy = Sys.Date()) {
  actual <- if (identical(tipoentidad, "FINRE")) .ultimo_trimestre_cerrado(hoy) else .ultimo_mes_cerrado(hoy)
  anterior <- retroceder_periodo(actual, tipoentidad)
  list(actual = actual, anterior = anterior)
}
