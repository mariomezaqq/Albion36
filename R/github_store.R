# =============================================================================
# ALMACEN DE DATOS (local + GitHub) -- copiado 1:1 del patron de Pulso-VCC.
# En la nube (shinyapps.io) no hay disco permanente: cada vez que el contenedor
# se reinicia, vuelve a los archivos que estaban en el bundle del ultimo
# rsconnect::deployApp(). Por eso "Actualizar universo" y la correccion manual
# de TAC se commitean al repo de GitHub via REST Contents API, y se
# resincronizan desde ahi al arrancar cada sesion -- asi la app siempre
# muestra la ULTIMA carga real, no la de hace varios dias.
#
# Config por variables de entorno (shinyapps.io -> Settings -> no hay UI en
# el plan Free, por eso el token va en R/secret_token.R, ver ese archivo):
#   GITHUB_TOKEN     -> PAT con permiso 'contents:write' al repo
#   GH_REPO          -> "usuario/repositorio" (default mariomezaqq/Albion36)
#   GH_BRANCH        -> rama (default "main")
#   ALBION_DATA_URL  -> (opcional) base raw para LEER
#
# Si no hay token/repo, todo funciona en local: lee y escribe en data/.
# =============================================================================
suppressMessages({ library(httr); library(jsonlite) })

.gh <- function() list(
  token  = Sys.getenv("GITHUB_TOKEN", ""),
  repo   = Sys.getenv("GH_REPO", "mariomezaqq/Albion36"),
  branch = Sys.getenv("GH_BRANCH", "main"),
  raw    = Sys.getenv("ALBION_DATA_URL", "https://raw.githubusercontent.com/mariomezaqq/Albion36/main")
)
gh_habilitado <- function() { g <- .gh(); nzchar(g$token) && nzchar(g$repo) }

# ---- LECTURA ----
store_path <- function(rel) file.path("data", rel)

store_existe <- function(rel) {
  if (file.exists(store_path(rel))) return(TRUE)
  g <- .gh(); if (!nzchar(g$raw)) return(FALSE)
  resp <- tryCatch(HEAD(paste0(g$raw, "/data/", rel), timeout(15)), error = function(e) NULL)
  !is.null(resp) && status_code(resp) == 200
}

#' Descarga un archivo del repo (raw) a data/ local, pisando lo que haya.
#' Se llama SIEMPRE (no solo si falta local) para que una sesion nueva
#' refleje lo que otra sesion/instancia guardo despues del ultimo deploy.
store_sync <- function(rel) {
  local <- store_path(rel)
  g <- .gh()
  if (nzchar(g$raw)) {
    resp <- tryCatch(GET(paste0(g$raw, "/data/", rel), timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      dir.create(dirname(local), showWarnings = FALSE, recursive = TRUE)
      writeBin(content(resp, "raw"), local)
    }
  }
  if (file.exists(local)) local else NULL
}

# ---- ESCRITURA ----
#' Guarda 'raw' (vector raw) en data/<rel> local y, si GitHub esta configurado,
#' lo commitea al repo. Devuelve un mensaje de estado.
store_write_bin <- function(rel, raw, mensaje = NULL) {
  local <- store_path(rel)
  dir.create(dirname(local), showWarnings = FALSE, recursive = TRUE)
  writeBin(raw, local)
  if (!gh_habilitado()) return(paste0("Guardado local: ", local, " (GitHub no configurado)"))

  g <- .gh()
  api <- paste0("https://api.github.com/repos/", g$repo, "/contents/data/", rel)
  hdr <- add_headers(Authorization = paste("token", g$token),
                     Accept = "application/vnd.github+json",
                     "User-Agent" = "albion-universo36-cloud")
  sha <- NULL
  r0 <- tryCatch(GET(api, hdr, query = list(ref = g$branch), timeout(20)), error = function(e) NULL)
  if (!is.null(r0) && status_code(r0) == 200)
    sha <- content(r0, "parsed")$sha
  body <- list(message = mensaje %||% paste("Actualizar data/", rel),
               content = jsonlite::base64_enc(raw), branch = g$branch)
  if (!is.null(sha)) body$sha <- sha
  r1 <- tryCatch(PUT(api, hdr, body = toJSON(body, auto_unbox = TRUE), timeout(30)),
                 error = function(e) NULL)
  if (!is.null(r1) && status_code(r1) %in% c(200, 201))
    paste0("Commit OK a GitHub: data/", rel)
  else
    paste0("Guardado local OK, pero FALLO el commit a GitHub (HTTP ",
           if (!is.null(r1)) status_code(r1) else "NULL", ").")
}

store_write_text <- function(rel, texto, mensaje = NULL)
  store_write_bin(rel, charToRaw(enc2utf8(paste(texto, collapse = "\n"))), mensaje)

store_write_file <- function(rel, ruta_local_origen, mensaje = NULL)
  store_write_bin(rel, readBin(ruta_local_origen, "raw", file.info(ruta_local_origen)$size), mensaje)

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
