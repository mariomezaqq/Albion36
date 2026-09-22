# =============================================================================
# RENTABILIDADES para la pestana "Comparar Carteras" -- MTD/1M/3M/YTD/12M por
# fondo, via el mismo scraper de valor cuota que usa "Ranking YTD"
# (pestania=7 de la CMF). RGFMU no necesita token reCAPTCHA (la mayoria del
# universo); solo FINRE/FIRES (ej. Albion) lo requiere -- si no hay token
# configurado, esos fondos simplemente quedan sin rentabilidad, no rompe el
# resto (mismo patron de degradacion que comparar_fondos()).
#
# Cache por dia (data/rentabilidades_cache.rds): valor cuota no cambia mas
# de una vez al dia, asi que comparar el mismo fondo varias veces en el
# mismo dia no vuelve a pegarle a la CMF.
# =============================================================================

# Rentabilidad simple entre dos fechas (primer/ultimo valor disponible en el
# rango). Vive aca (no en app.R) a proposito: app.R corre su codigo de nivel
# superior en un entorno propio de Shiny, separado del .GlobalEnv real donde
# quedan las funciones de los R/*.R sourceados -- si esta funcion se define
# inline en app.R, las funciones sourceadas (como obtener_rentabilidades_fondo()
# de mas abajo) NO la ven al llamarla (error "no se pudo encontrar la funcion
# .rent", confirmado en vivo). actualizar_universo() en app.R tambien la usa y
# SI la ve desde aca porque su entorno es hijo del .GlobalEnv.
.rent <- function(h, desde, hasta) {
  if (is.null(h) || !nrow(h)) return(NA_real_)
  ini <- h %>% dplyr::filter(fecha >= desde) %>% dplyr::arrange(fecha) %>% dplyr::slice(1)
  fin <- h %>% dplyr::filter(fecha <= hasta) %>% dplyr::arrange(dplyr::desc(fecha)) %>% dplyr::slice(1)
  if (!nrow(ini) || !nrow(fin) || ini$valor_cuota_ajustado <= 0) return(NA_real_)
  fin$valor_cuota_ajustado / ini$valor_cuota_ajustado - 1
}

RUTA_RENT_CACHE <- file.path("data", "rentabilidades_cache.rds")

cargar_rent_cache <- function() {
  if (file.exists(RUTA_RENT_CACHE)) tryCatch(readRDS(RUTA_RENT_CACHE), error = function(e) list()) else list()
}
guardar_rent_cache <- function(cache) {
  dir.create("data", showWarnings = FALSE)
  saveRDS(cache, RUTA_RENT_CACHE)
}

.clave_rent <- function(fondo) paste(fondo$nombre, Sys.Date())

#' Rentabilidades MTD/1M/3M/YTD/12M de UN fondo, con cache diario.
#' @return list(rent = list(m1, m3, ytd, m12), cache = cache actualizado)
obtener_rentabilidades_fondo <- function(fondo, cred, cache) {
  clave <- .clave_rent(fondo)
  if (!is.null(cache[[clave]])) return(list(rent = cache[[clave]], cache = cache))

  hasta <- Sys.Date() - 1
  desde <- hasta - 380
  ref_ytd <- as.Date(sprintf("%d-12-31", lubridate::year(hasta) - 1))

  h <- tryCatch(scrapear_fondo(fondo, desde, hasta, cred$token, cred$cookies),
                error = function(e) { message("[rentabilidades] ERROR ", fondo$nombre, ": ", e$message); NULL })
  if (!is.null(h) && nrow(h) > 0) h <- aplicar_factor_reparto(h)

  desde_mtd <- as.Date(format(hasta, "%Y-%m-01"))
  rent <- list(
    mtd = .rent(h, desde_mtd, hasta),
    m1  = .rent(h, hasta - 30, hasta),
    m3  = .rent(h, hasta - 91, hasta),
    ytd = .rent(h, ref_ytd, hasta),
    m12 = .rent(h, hasta - 365, hasta)
  )
  cache[[clave]] <- rent
  list(rent = rent, cache = cache)
}

#' Rentabilidades de una lista de fondos seleccionados. Entry point para app.R.
#' @param fondos_seleccionados sublista de FONDOS_UNIVERSO36
#' @param progreso callback opcional function(v, detail)
#' @return named list (por fondo$nombre) con list(m1, m3, ytd, m12)
obtener_rentabilidades <- function(fondos_seleccionados, progreso = NULL) {
  cred <- tryCatch(cargar_credenciales(), error = function(e) {
    message("[rentabilidades] sin credenciales CMF configuradas (", e$message, "); ",
            "se continua sin token -- no afecta RGFMU, solo fondos FIRES/FINRE.")
    list(token = NULL, cookies = "")
  })
  cache <- cargar_rent_cache()
  n <- length(fondos_seleccionados)
  out <- list()
  for (i in seq_along(fondos_seleccionados)) {
    fondo <- fondos_seleccionados[[i]]
    if (!is.null(progreso)) progreso(i / n, detail = paste("Rentabilidad:", fondo$nombre))
    r <- obtener_rentabilidades_fondo(fondo, cred, cache)
    cache <- r$cache
    out[[fondo$nombre]] <- r$rent
    Sys.sleep(0.3)
  }
  guardar_rent_cache(cache)
  out
}
