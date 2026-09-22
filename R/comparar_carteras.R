# =============================================================================
# ORQUESTACION: comparacion de carteras entre fondos
# Punto de entrada unico que usa app.R. Junta cartera_cmf.R + periodos_cartera.R
# + directorio_fondos.R, calcula el diff entre los dos periodos mas recientes
# de cada fondo, y cachea resultados en disco (mismo patron que RUTA_RDS/
# cargar_cache() en app.R).
# =============================================================================
suppressMessages({ library(dplyr); library(tibble) })

RUTA_RDS_CARTERAS <- file.path("data", "carteras_universo36.rds")

# Umbral para no marcar como "Subio"/"Bajo" el ruido de punto flotante (NO de
# redondeo de la fuente -- la CMF ya reporta con 2 decimales, asi que
# cualquier delta distinto de cero entre dos periodos es un cambio real).
# Antes este umbral era 0.05 (el mismo orden de magnitud que un cambio real
# tipico), lo que ademas de ser demasiado agresivo caia justo en el limite de
# error de punto flotante: 0.22 - 0.17 da 0.049999999999999989 en double,
# que NO es > 0.05, y el cambio se perdia como "Sin cambio" pese a ser real
# (caso reportado: Falabella 0.17% -> 0.22%). delta_pct_activo se redondea a
# 6 decimales antes de comparar contra este umbral.
EPS_DIFF <- 1e-6

cargar_cache_carteras <- function() {
  if (file.exists(RUTA_RDS_CARTERAS)) tryCatch(readRDS(RUTA_RDS_CARTERAS), error = function(e) list()) else list()
}

guardar_cache_carteras <- function(cache) {
  dir.create("data", showWarnings = FALSE)
  saveRDS(cache, RUTA_RDS_CARTERAS)
}

.clave_cache <- function(fondo, periodo) paste(fondo$nombre, periodo$label)

#' Trae (con cache) la cartera consolidada + nombre de emisor resuelto de UN
#' fondo para UN periodo especifico. La clasificacion `clase` NO se cachea
#' aca a proposito -- se recalcula siempre en obtener_cartera_comparativa()
#' con las reglas/overrides vigentes, para que ajustar la heuristica de
#' clasificacion no requiera borrar la cache de red (que es lo caro: scraping
#' CMF + resolucion de nombre de emisor).
#' @return list(tabla = tibble, cache = cache actualizado)
.obtener_cartera_periodo_cacheada <- function(fondo, periodo, cred, dir_cfm, dir_cfi, cache) {
  clave <- .clave_cache(fondo, periodo)
  if (!is.null(cache[[clave]])) return(list(tabla = cache[[clave]], cache = cache))

  tabla <- obtener_cartera_fondo_periodo(fondo, periodo, cred)
  tabla <- resolver_nombre_emisor(tabla, dir_cfm, dir_cfi)
  cache[[clave]] <- tabla
  list(tabla = tabla, cache = cache)
}

#' Cartera de UN fondo, con fallback: si el periodo "actual" viene vacio
#' (publicacion con rezago), retrocede un periodo mas (una sola vez, para no
#' multiplicar requests indefinidamente).
#'
#' Los fondos FINRE (ej. Albion, unico de este tipo en el universo) publican
#' cartera TRIMESTRAL con rezago considerable -- comparar contra el trimestre
#' anterior rara vez aporta y duplica requests a la CMF sin necesidad. Para
#' estos fondos solo se trae el ultimo periodo disponible (modo "snapshot",
#' `es_snapshot = TRUE`, `tabla_anterior` vacia). Los RGFMU (mensuales) siguen
#' trayendo los 2 periodos para la comparacion de cambios.
#' @return list(fondo, es_snapshot, periodo_actual, periodo_anterior,
#'         tabla_actual, tabla_anterior, cache)
obtener_cartera_comparativa <- function(fondo, cred, dir_cfm, dir_cfi, cache = list()) {
  es_snapshot <- identical(fondo$tipoentidad, "FINRE")
  periodos <- calcular_periodos_recientes(fondo$tipoentidad)

  r_actual <- .obtener_cartera_periodo_cacheada(fondo, periodos$actual, cred, dir_cfm, dir_cfi, cache)
  cache <- r_actual$cache

  if (nrow(r_actual$tabla) == 0) {
    message("[comparar_carteras] ", fondo$nombre, ": periodo ", periodos$actual$label,
            " vacio, retrocediendo un periodo mas.")
    periodos$actual <- retroceder_periodo(periodos$actual, fondo$tipoentidad)
    periodos$anterior <- retroceder_periodo(periodos$anterior, fondo$tipoentidad)
    r_actual <- .obtener_cartera_periodo_cacheada(fondo, periodos$actual, cred, dir_cfm, dir_cfi, cache)
    cache <- r_actual$cache
  }

  overrides <- cargar_clase_overrides()

  if (es_snapshot) {
    return(list(fondo = fondo$nombre, es_snapshot = TRUE,
                periodo_actual = periodos$actual, periodo_anterior = NULL,
                tabla_actual = clasificar_clase(r_actual$tabla, overrides), tabla_anterior = tibble(), cache = cache))
  }

  r_anterior <- .obtener_cartera_periodo_cacheada(fondo, periodos$anterior, cred, dir_cfm, dir_cfi, cache)
  cache <- r_anterior$cache

  list(fondo = fondo$nombre, es_snapshot = FALSE,
       periodo_actual = periodos$actual, periodo_anterior = periodos$anterior,
       tabla_actual = clasificar_clase(r_actual$tabla, overrides),
       tabla_anterior = clasificar_clase(r_anterior$tabla, overrides), cache = cache)
}

#' Compara dos tablas de cartera consolidadas (mismo fondo, dos periodos) y
#' marca que cambio entre una y otra.
#' @return tibble: nombre_emisor, categoria, tipo_instrumento,
#'   valorizacion_actual, valorizacion_anterior, pct_activo_actual,
#'   pct_activo_anterior, delta_valorizacion, delta_pct_activo, estado
#'   estado in c("Nuevo","Salido","Subio","Bajo","Sin cambio")
diff_carteras <- function(tabla_actual, tabla_anterior) {
  vacia <- tibble(nemotecnico = character(), rut_emisor = character(), categoria = character(),
                   tipo_instrumento = character(), nombre_emisor = character(), clase = character(),
                   valorizacion_cierre = numeric(), pct_activo_fondo = numeric())
  a <- if (is.null(tabla_actual) || nrow(tabla_actual) == 0) vacia else tabla_actual
  b <- if (is.null(tabla_anterior) || nrow(tabla_anterior) == 0) vacia else tabla_anterior
  if (nrow(a) == 0 && nrow(b) == 0) return(vacia %>% mutate(estado = character()))

  # Filas sin nemotecnico NI rut_emisor (derivados con header de 2 niveles no
  # soportado, ver .limpiar_cartera_generico) no tienen clave confiable para
  # emparejar entre periodos -- se excluyen del diff para evitar falsos
  # matches many-to-many; siguen visibles en la tabla de holdings del periodo.
  .con_clave <- function(df) filter(df, (!is.na(nemotecnico) & nemotecnico != "") | (!is.na(rut_emisor) & rut_emisor != ""))
  clave <- c("nemotecnico", "rut_emisor", "categoria")
  # Un mismo instrumento puede aparecer en varios "lotes" (filas repetidas con
  # el mismo nemotecnico, ej. compras del mismo bono en distintas fechas) --
  # hay que consolidarlos a UNA fila por instrumento ANTES del join. Si no,
  # full_join empareja cada lote del periodo actual con cada lote del periodo
  # anterior (producto cartesiano), multiplicando MM$/% activo y disparando
  # el TOTAL muy por sobre el 100% (confirmado con datos reales: fondo con
  # bonos repetidos en 10+ lotes inflaba el TOTAL a 600%+).
  .consolidar_lotes <- function(df) {
    df %>%
      group_by(across(all_of(c(clave, "nombre_emisor", "tipo_instrumento", "clase")))) %>%
      summarise(valorizacion_cierre = sum(valorizacion_cierre, na.rm = TRUE),
                pct_activo_fondo = sum(pct_activo_fondo, na.rm = TRUE), .groups = "drop")
  }
  a2 <- .con_clave(a) %>% mutate(across(all_of(clave), ~ ifelse(is.na(.) | . == "", "(sin dato)", .))) %>% .consolidar_lotes()
  b2 <- .con_clave(b) %>% mutate(across(all_of(clave), ~ ifelse(is.na(.) | . == "", "(sin dato)", .))) %>% .consolidar_lotes()

  full_join(a2, b2, by = clave, suffix = c("_actual", "_anterior")) %>%
    mutate(
      nombre_emisor = dplyr::coalesce(nombre_emisor_actual, nombre_emisor_anterior),
      tipo_instrumento = dplyr::coalesce(tipo_instrumento_actual, tipo_instrumento_anterior),
      clase = dplyr::coalesce(clase_actual, clase_anterior),
      valorizacion_actual = valorizacion_cierre_actual,
      valorizacion_anterior = valorizacion_cierre_anterior,
      pct_activo_actual = pct_activo_fondo_actual,
      pct_activo_anterior = pct_activo_fondo_anterior,
      delta_valorizacion = valorizacion_actual - valorizacion_anterior,
      delta_pct_activo = round(pct_activo_actual - pct_activo_anterior, 6),
      estado = dplyr::case_when(
        is.na(valorizacion_anterior) ~ "Nuevo",
        is.na(valorizacion_actual) ~ "Salido",
        is.na(delta_pct_activo) ~ "Sin cambio",
        delta_pct_activo > EPS_DIFF ~ "Subio",
        delta_pct_activo < -EPS_DIFF ~ "Bajo",
        TRUE ~ "Sin cambio"
      )
    ) %>%
    select(nemotecnico, rut_emisor, categoria, nombre_emisor, tipo_instrumento, clase,
           valorizacion_actual, valorizacion_anterior, pct_activo_actual, pct_activo_anterior,
           delta_valorizacion, delta_pct_activo, estado) %>%
    arrange(factor(estado, levels = c("Nuevo", "Salido", "Subio", "Bajo", "Sin cambio")),
            desc(abs(delta_pct_activo)))
}

#' Corre obtener_cartera_comparativa() sobre una lista de fondos seleccionados.
#' Entry point unico que usa app.R.
#' @param fondos_seleccionados sublista de FONDOS_UNIVERSO36
#' @param progreso callback opcional function(v, detail) para withProgress/setProgress
#' @return named list (por fondo$nombre), cada elemento con $diff agregado
comparar_fondos <- function(fondos_seleccionados, progreso = NULL) {
  cred <- tryCatch(cargar_credenciales(), error = function(e) {
    message("[comparar_carteras] sin credenciales CMF configuradas (", e$message, "); ",
            "se continua sin token -- no afecta RGFMU ni FINRE, solo fondos FIRES.")
    list(token = NULL, cookies = "")
  })
  dir_cfm <- obtener_directorio_cfm()
  dir_cfi <- obtener_directorio_cfi()
  cache <- cargar_cache_carteras()

  n <- length(fondos_seleccionados)
  resultados <- list()
  for (i in seq_along(fondos_seleccionados)) {
    fondo <- fondos_seleccionados[[i]]
    if (!is.null(progreso)) progreso(i / n, detail = fondo$nombre)
    res <- obtener_cartera_comparativa(fondo, cred, dir_cfm, dir_cfi, cache)
    cache <- res$cache
    res$diff <- if (res$es_snapshot) NULL else diff_carteras(res$tabla_actual, res$tabla_anterior)
    resultados[[fondo$nombre]] <- res
  }

  guardar_cache_carteras(cache)
  resultados
}
