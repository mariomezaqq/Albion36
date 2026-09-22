# =============================================================================
# ALBION vs. UNIVERSO 36 - Shiny app
# Recreacion local de https://mariomezaqq.shinyapps.io/albion-universo36/
# (ese deploy se hizo desde otro PC y nunca se subio a GitHub; esta es una
# reconstruccion del esqueleto reutilizando el scraper CMF de Pulso-VCC).
#
# CFIALBIONA vs. universo de fondos conservadores: usa SIEMPRE la serie de
# MAYOR TAC de cada fondo, valor cuota ajustado por factor de reparto (RGFMU).
# Fuente: CMF (mismo scraper del Pulso/Albion).
# =============================================================================
suppressMessages({
  library(shiny); library(httr); library(rvest); library(dplyr)
  library(stringr); library(lubridate); library(tibble); library(DT)
})

base <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
setwd(base)
# github_store va PRIMERO: en la nube, cada contenedor nuevo arranca desde el
# bundle del ultimo deploy (datos viejos); esto baja la ULTIMA carga real
# desde GitHub antes de que cargar_cache()/cargar_tac_manual() lean data/.
source("R/github_store.R")
try(store_sync("series_universo36.rds"), silent = TRUE)
try(store_sync("tac_manual.csv"), silent = TRUE)
if (file.exists("R/secret_token.R")) try(source("R/secret_token.R"), silent = TRUE)
source("R/credenciales.R")
source("R/scraper.R")
source("R/fondos_universo36.R")
source("R/ajustes_rgfmu.R")
source("R/periodos_cartera.R")
source("R/cartera_cmf.R")
source("R/directorio_fondos.R")
source("R/comparar_carteras.R")
source("R/clasificacion_yahoo.R")
source("R/clasificacion_clase.R")
source("R/graficos_cartera.R")
source("R/rentabilidades_comparar.R")
source("R/ranking_universo.R")
source("R/ui_theme.R")
suppressMessages({ library(ggplot2); library(scales); library(plotly) })

RUTA_RDS <- file.path("data", "series_universo36.rds")

# Colores del tema para colorear celdas en las tablas DT (mismos valores que TEMA_CSS)
.COL_POS <- "#1a7f4b"; .COL_POS_BG <- "#eaf6f0"
.COL_NEG <- "#c0392b"; .COL_NEG_BG <- "#fdf0ef"
.COL_MUTED <- "#7a869a"

# Categorias abreviadas y valorizacion en millones (MM$) para que las tablas
# de comparacion de carteras queden angostas y quepa mas de una por fila.
.ABREV_CATEGORIA <- c(
  "Nacional" = "Nac.", "Extranjera" = "Ext.",
  "En opciones" = "Opc.", "En Opciones" = "Opc.",
  "En futuros y forward" = "Fut./Fwd.", "En Futuros y Forward" = "Fut./Fwd.",
  "En opciones lanzador" = "Opc. Lanz.",
  "Metodo de Participacion" = "Mét. Part.", "Bienes Raices" = "Bienes Raíces"
)
.abrev_categoria <- function(x) dplyr::coalesce(unname(.ABREV_CATEGORIA[x]), x)
.mm <- function(x) ifelse(is.na(x), "-", formatC(x / 1000, format = "f", digits = 1, big.mark = ".", decimal.mark = ","))

.ABREV_CLASE <- c("RV Nacional" = "RV Nac.", "RV Internacional" = "RV Int.",
                   "RF Nacional" = "RF Nac.", "RF Internacional" = "RF Int.",
                   "Alternativos Nacional" = "Alt. Nac.", "Alternativos Internacional" = "Alt. Int.",
                   "Balanceados Nacional" = "Bal. Nac.", "Balanceados Internacional" = "Bal. Int.",
                   "Sin Clasificar" = "S/Clas.")
.abrev_clase <- function(x) dplyr::coalesce(unname(.ABREV_CLASE[x]), x)

# Formatea una rentabilidad (fraccion, ej 0.034) a texto + color para las
# tarjetas de Comparar Carteras. "s/d" si no hay dato (ej. fondo FIRES/FINRE
# sin token reCAPTCHA configurado).
.fmt_rent <- function(x) if (is.na(x)) "s/d" else sprintf("%+.2f%%", x * 100)
.col_rent <- function(x) if (is.na(x)) .COL_MUTED else if (x >= 0) .COL_POS else .COL_NEG
# Version vectorizada de .fmt_rent(), para usar dentro de transmute() sobre
# columnas enteras (ej. tabla de Ranking YTD) -- .fmt_rent() de arriba espera
# UN solo valor a la vez (if(is.na(x)) rompe con "the condition has length > 1"
# si x es un vector, confirmado en vivo al armar la tabla de 33 fondos).
.fmt_rent_vec <- function(x) ifelse(is.na(x), "s/d", sprintf("%+.2f%%", x * 100))

# Orden de filas dentro de cada tabla de cartera: Nacional, Extranjera, despues
# el resto de categorias; dentro de cada categoria, por Tipo Instrumento; y
# dentro de eso, por % del activo del fondo del periodo actual (desc).
.CATEGORIA_ORDEN <- c("Nacional" = 1, "Extranjera" = 2, "En opciones" = 3, "En Opciones" = 3,
                      "En futuros y forward" = 4, "En Futuros y Forward" = 4,
                      "En opciones lanzador" = 5, "Metodo de Participacion" = 6, "Bienes Raices" = 7)
.orden_categoria <- function(x) { o <- unname(.CATEGORIA_ORDEN[x]); ifelse(is.na(o), 99, o) }

# Construye una DT con la fila TOTAL como <tfoot> real en vez de una fila mas
# de datos -- asi queda fija abajo, inmune al buscador y al sort por columna
# de DataTables (con la fila normal, al ordenar/filtrar por Clase se mezclaba
# con el resto y hasta podia terminar arriba).
.tabla_con_total <- function(tabla, footer_vals, options) {
  sketch <- htmltools::withTags(table(
    class = "display",
    thead(tr(lapply(names(tabla), th))),
    tfoot(tr(lapply(footer_vals, th)))
  ))
  DT::datatable(tabla, container = sketch, rownames = FALSE, options = options, class = "stripe hover compact")
}

# ---- Scrapea todo el universo (34 fondos): tabla rica de Ranking (ver
# R/ranking_universo.R) -- un fetch por fondo, sin precargar carteras (eso
# se probo y dejaba el proceso demasiado lento, se revirtio a proposito).
actualizar_universo <- function(progreso = NULL) {
  cred  <- cargar_credenciales()
  hasta <- Sys.Date() - 1

  n <- length(FONDOS_UNIVERSO36)
  filas <- list()
  for (i in seq_along(FONDOS_UNIVERSO36)) {
    fd <- FONDOS_UNIVERSO36[[i]]
    if (!is.null(progreso)) progreso(i / n, detail = fd$nombre)
    filas[[fd$nombre]] <- calcular_fila_ranking(fd, cred, hasta)
    Sys.sleep(0.6)
  }

  tabla <- bind_rows(lapply(filas, as_tibble)) %>% arrange(desc(mtd))
  datos <- list(generado = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "America/Santiago"), tabla = tabla)
  saveRDS(datos, RUTA_RDS)
  # Commitea a GitHub (ver R/github_store.R): sin esto, el proximo contenedor
  # que arranque en shinyapps.io vuelve a ver el bundle viejo del deploy.
  msg <- tryCatch(store_write_file("series_universo36.rds", RUTA_RDS,
                                   mensaje = paste("Actualizar universo:", datos$generado)),
                  error = function(e) paste("FALLO commit a GitHub:", e$message))
  message("[actualizar_universo] ", msg)
  datos$gh_msg <- msg
  datos
}

cargar_cache <- function() if (file.exists(RUTA_RDS)) tryCatch(readRDS(RUTA_RDS), error = function(e) NULL) else NULL

# ---- UI ----
ui <- fluidPage(
  tags$head(tags$style(HTML(TEMA_CSS))),
  au_header("Albion vs. Universo — Fondos Conservadores"),

  tabsetPanel(id = "main_tabs", type = "pills",

    # Ranking YTD primero (pestaña principal, formato igual al de siempre) --
    # unico agregado: seleccionar filas + boton para llevarlas a comparar
    # carteras en la otra pestaña, el resto (tabla, texto, boton actualizar)
    # queda identico a como estaba.
    tabPanel("Ranking YTD",
      div(style = "margin-top:18px",
        p(class = "au-hint", paste0(
          max(length(FONDOS_UNIVERSO36) - 1, 0),
          " fondos mutuos conservadores de administradoras chilenas + Albion. De CADA fondo se usa SIEMPRE ",
          "la serie de MAYOR TAC (\"la mas cara\"), tal como aparece en el buscador de fondos. ",
          "Valor cuota ajustado por factor de reparto (RGFMU). Fuente: CMF (mismo scraper del Pulso/Albion).")),
        div(class = "au-toolbar",
          actionButton("btn_actualizar", sprintf("Actualizar universo (%d fondos)", length(FONDOS_UNIVERSO36))),
          span(style = "margin-left:10px;color:var(--muted);font-size:12px;",
               "tarda ~1 min (scrapes secuenciales a la CMF, uno por uno).")
        ),
        uiOutput("estado"),
        uiOutput("cards_ranking"),
        p(class = "au-hint",
          "Selecciona una o mas filas (click) y presiona el boton para comparar sus carteras en la otra pestana. ",
          "Doble click en \"TAC serie %\" para corregirla (unica columna editable)."),
        div(class = "au-toolbar", actionButton("btn_ir_comparar", "Comparar seleccionados →")),
        div(class = "au-table-wrap", DTOutput("tabla_resultado"))
      )
    ),

    tabPanel("Comparar Carteras",
      div(style = "margin-top:18px",
        p(class = "au-hint",
          "Compara los holdings (cartera de inversiones) de cualquier combinacion de fondos del ",
          "universo. Para cada fondo se muestran, en una sola tabla, su periodo mas reciente publicado ",
          "en la CMF y el anterior (mensual para fondos mutuos RGFMU, trimestral para fondos de ",
          "inversion FINRE como Albion), con cuanto subio o bajo cada posicion o si es nueva."),
        if (length(FONDOS_UNIVERSO36) <= 1)
          div(class = "alerta",
              "Por ahora solo esta cargado FI Albion en el catalogo de fondos -- los 33 fondos ",
              "mutuos conservadores todavia estan pendientes de cargar. En cuanto se agreguen ",
              "van a aparecer aca automaticamente para comparar."),
        selectizeInput("fondos_comparar", "Fondos a comparar",
                        choices = vapply(FONDOS_UNIVERSO36, `[[`, "", "nombre"),
                        selected = "FI Albion", multiple = TRUE),
        div(class = "au-toolbar", actionButton("btn_comparar", "Comparar")),
        uiOutput("resultados_comparacion")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # ---- Comparar Carteras ----
  resultados_cmp <- reactiveVal(NULL)
  rentabilidades_cmp <- reactiveVal(NULL)

  # Compartido entre el boton "Comparar" de esta pestana y el boton "Comparar
  # seleccionados" de Ranking YTD (ver mas abajo) -- ambos disparan el mismo
  # flujo: carteras + rentabilidades de los fondos elegidos.
  .ejecutar_comparacion <- function(fondos_sel) {
    withProgress(message = "Consultando carteras y rentabilidades en la CMF...", value = 0, {
      res <- tryCatch(
        comparar_fondos(fondos_sel, progreso = function(v, detail) setProgress(value = v * 0.7, detail = detail)),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
      if (!is.null(res)) resultados_cmp(res)
      rent <- tryCatch(
        obtener_rentabilidades(fondos_sel, progreso = function(v, detail) setProgress(value = 0.7 + v * 0.3, detail = detail)),
        error = function(e) { message("[rentabilidades] error: ", e$message); NULL }
      )
      rentabilidades_cmp(rent)
    })
  }

  observeEvent(input$btn_comparar, {
    req(input$fondos_comparar)
    fondos_sel <- Filter(function(f) f$nombre %in% input$fondos_comparar, FONDOS_UNIVERSO36)
    .ejecutar_comparacion(fondos_sel)
  })

  output$resultados_comparacion <- renderUI({
    res <- resultados_cmp()
    if (is.null(res) || length(res) == 0) return(p(class = "au-hint", "Selecciona uno o mas fondos y presiona \"Comparar\"."))
    rent <- rentabilidades_cmp()
    # Ranking de cada fondo comparado. Para YTD, contra el universo completo
    # de 36 fondos (usa la foto de "Actualizar universo" en Ranking YTD, que
    # solo trae YTD -- ver nota en actualizar_universo()). Para MTD/1M/3M/12M,
    # que no estan en esa foto, el ranking queda acotado a los pocos fondos
    # que se estan comparando ahora mismo (no hay universo con que compararlos).
    periodos_rent <- list(c("MTD", "mtd"), c("1M", "m1"), c("3M", "m3"), c("YTD", "ytd"), c("12M", "m12"))
    universo_rent <- cargar_cache()
    ranking_rent <- setNames(lapply(periodos_rent, function(p) {
      campo <- p[2]
      tabla_u <- if (!is.null(universo_rent)) universo_rent$tabla else NULL
      vals <- if (!is.null(tabla_u) && campo %in% names(tabla_u) && nrow(tabla_u) > 1)
        setNames(tabla_u[[campo]], tabla_u$nombre)
      else if (!is.null(rent))
        vapply(rent, function(r) if (is.null(r)) NA_real_ else r[[campo]], numeric(1))
      else return(NULL)
      if (!is.null(rent)) for (nf in names(rent)) {
        if (nf %in% names(vals) && !is.null(rent[[nf]][[campo]]) && !is.na(rent[[nf]][[campo]])) vals[nf] <- rent[[nf]][[campo]]
      }
      list(rank = rank(-vals, na.last = "keep", ties.method = "min"), n = sum(!is.na(vals)))
    }), vapply(periodos_rent, `[`, "", 2))
    tagList(
      div(class = "au-cartera-grid", lapply(names(res), function(nombre_fondo) {
      id_base <- make.names(nombre_fondo)
      r <- res[[nombre_fondo]]
      periodos_txt <- if (isTRUE(r$es_snapshot))
        sprintf("Cartera al %s (%d instr.) — trimestral con rezago, solo ultimo periodo.",
                r$periodo_actual$label, nrow(r$tabla_actual))
        else sprintf("Actual: %s (%d) — Anterior: %s (%d)",
                     r$periodo_actual$label, nrow(r$tabla_actual),
                     r$periodo_anterior$label, nrow(r$tabla_anterior))
      rf <- if (!is.null(rent)) rent[[nombre_fondo]] else NULL
      div(class = "au-cartera-item",
        div(class = "au-fondo-title", nombre_fondo),
        div(class = "au-periodos", periodos_txt),
        if (!is.null(rf))
          div(class = "au-rentabilidades", lapply(periodos_rent, function(p) {
            campo <- p[2]; valor <- rf[[campo]]
            # ojo: [[nombre_fondo]] sobre un vector nombrado tira "subindice
            # fuera de los limites" si el nombre no esta (ej. un fondo recien
            # agregado a FONDOS_UNIVERSO36 que todavia no aparece en la ultima
            # foto de "Actualizar universo") -- con [nombre_fondo] (corchete
            # simple) devuelve NA de forma segura en ese caso.
            rk <- if (!is.null(ranking_rent)) unname(ranking_rent[[campo]]$rank[nombre_fondo]) else NA
            n  <- if (!is.null(ranking_rent)) ranking_rent[[campo]]$n else NA
            div(class = "au-rent-item",
              div(paste0(p[1], ": "), tags$b(style = paste0("color:", .col_rent(valor)), .fmt_rent(valor))),
              if (!is.na(rk) && !is.na(valor)) div(class = "au-rent-rank", sprintf("#%d de %d", rk, n))
            )
          })),
        div(class = "au-graficos-row",
          plotly::plotlyOutput(paste0("plot_categoria_", id_base), height = "260px"),
          plotly::plotlyOutput(paste0("plot_tipo_", id_base), height = "260px")
        ),
        div(class = "au-graficos-row au-graficos-row-1",
          plotly::plotlyOutput(paste0("plot_clase_", id_base), height = "260px")
        ),
        div(class = "au-table-wrap", DTOutput(paste0("tabla_diff_", id_base)))
      )
    })))
  })

  observe({
    res <- resultados_cmp()
    req(res)
    for (nombre_fondo in names(res)) {
      local({
        nf <- nombre_fondo
        id_base <- make.names(nf)
        r <- res[[nf]]

        output[[paste0("plot_categoria_", id_base)]] <- plotly::renderPlotly({
          grafico_torta(agregar_categoria_fondo(r$tabla_actual), "categoria_simple", "Nacional / Extranjera")
        })

        output[[paste0("plot_tipo_", id_base)]] <- plotly::renderPlotly({
          grafico_torta(agregar_tipo_fondo(r$tabla_actual), "tipo_instrumento", "Tipo de Instrumento")
        })

        output[[paste0("plot_clase_", id_base)]] <- plotly::renderPlotly({
          grafico_torta(agregar_clase_fondo(r$tabla_actual), "clase", "Clase")
        })

        output[[paste0("tabla_diff_", id_base)]] <- renderDT({

          # ---- Fondos FINRE (ej. Albion): solo snapshot del ultimo periodo, sin comparacion ----
          if (isTRUE(r$es_snapshot)) {
            if (!nrow(r$tabla_actual)) return(DT::datatable(tibble(Aviso = "Sin datos publicados para este periodo."),
                                                              rownames = FALSE, options = list(dom = "t")))
            tabla_snap <- r$tabla_actual %>%
              arrange(.orden_categoria(categoria), tipo_instrumento, desc(dplyr::coalesce(pct_activo_fondo, -1))) %>%
              transmute(
                Emisor = nombre_emisor,
                Cat. = .abrev_categoria(categoria),
                Tipo = tipo_instrumento,
                Clase = .abrev_clase(clase),
                `MM$` = .mm(valorizacion_cierre),
                `% Fondo` = ifelse(is.na(pct_activo_fondo), "-", sprintf("%.2f%%", pct_activo_fondo))
              )
            footer_vals <- c(
              "TOTAL", "", "", "",
              .mm(sum(r$tabla_actual$valorizacion_cierre, na.rm = TRUE)),
              sprintf("%.2f%%", sum(r$tabla_actual$pct_activo_fondo, na.rm = TRUE))
            )
            return(.tabla_con_total(tabla_snap, footer_vals,
              options = list(dom = "ft", pageLength = 200, order = list(), scrollX = TRUE)))
          }

          # ---- Fondos RGFMU: comparacion entre periodo actual y anterior ----
          if (!nrow(r$diff)) return(DT::datatable(tibble(Aviso = "Sin instrumentos identificables en ninguno de los dos periodos."),
                                                    rownames = FALSE, options = list(dom = "t")))
          tabla <- r$diff %>%
            arrange(.orden_categoria(categoria), tipo_instrumento,
                    desc(dplyr::coalesce(pct_activo_actual, -1))) %>%
            transmute(
              Emisor = nombre_emisor,
              Cat. = .abrev_categoria(categoria),
              Tipo = tipo_instrumento,
              Clase = .abrev_clase(clase),
              `MM$ ant.` = .mm(valorizacion_anterior),
              `MM$ act.` = .mm(valorizacion_actual),
              `% ant.` = ifelse(is.na(pct_activo_anterior), "-", sprintf("%.2f%%", pct_activo_anterior)),
              `% act.` = ifelse(is.na(pct_activo_actual), "-", sprintf("%.2f%%", pct_activo_actual)),
              Cambio = dplyr::case_when(
                estado == "Nuevo" ~ "▲ Nueva",
                estado == "Salido" ~ "▼ Salió",
                estado == "Sin cambio" ~ "=",
                TRUE ~ sprintf("%+.2f pp", delta_pct_activo)
              ),
              estado = estado
            )
          footer_vals <- c(
            "TOTAL", "", "", "",
            .mm(sum(r$diff$valorizacion_anterior, na.rm = TRUE)),
            .mm(sum(r$diff$valorizacion_actual, na.rm = TRUE)),
            sprintf("%.2f%%", sum(r$diff$pct_activo_anterior, na.rm = TRUE)),
            sprintf("%.2f%%", sum(r$diff$pct_activo_actual, na.rm = TRUE)),
            "", ""
          )

          .tabla_con_total(tabla, footer_vals,
            options = list(dom = "ft", pageLength = 200, order = list(), scrollX = TRUE,
                            columnDefs = list(list(visible = FALSE, targets = which(names(tabla) == "estado") - 1)))
          ) %>%
            DT::formatStyle("Cambio", valueColumns = "estado",
              color = DT::styleEqual(c("Nuevo", "Salido", "Subio", "Bajo", "Sin cambio"),
                                      c(.COL_POS, .COL_NEG, .COL_POS, .COL_NEG, .COL_MUTED)),
              backgroundColor = DT::styleEqual(c("Nuevo", "Salido", "Subio", "Bajo", "Sin cambio"),
                                                c(.COL_POS_BG, .COL_NEG_BG, "transparent", "transparent", "transparent")),
              fontWeight = DT::styleEqual(c("Nuevo", "Salido", "Subio", "Bajo", "Sin cambio"),
                                           c("700", "700", "700", "700", "400"))
            )
        })
      })
    }
  })

  # ---- Ranking YTD ----
  # Resincroniza desde GitHub al abrir cada sesion (no solo al arrancar el
  # proceso R): si el proceso lleva rato vivo y otra sesion/instancia guardo
  # una actualizacion mientras tanto, esta sesion nueva ve igual la ultima
  # carga real en vez de la que tenia el proceso al arrancar.
  try(store_sync("series_universo36.rds"), silent = TRUE)
  try(store_sync("tac_manual.csv"), silent = TRUE)
  estado <- reactiveVal(cargar_cache())

  observeEvent(input$btn_actualizar, {
    withProgress(message = "Consultando la CMF...", value = 0, {
      datos <- tryCatch(
        actualizar_universo(progreso = function(v, detail) setProgress(value = v, detail = detail)),
        error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL }
      )
      if (!is.null(datos)) {
        estado(datos)
        if (!is.null(datos$gh_msg))
          showNotification(datos$gh_msg,
                           type = if (grepl("FALLO", datos$gh_msg)) "warning" else "message",
                           duration = 8)
      }
    })
  })

  output$estado <- renderUI({
    d <- estado()
    if (is.null(d)) return(p(class = "au-hint", "Sin datos todavia. Presiona \"Actualizar universo\"."))
    p(class = "au-hint", paste("Ultima actualizacion:", d$generado))
  })

  # ---- TAC serie % -- unica columna editable a mano (no viene de la CMF,
  # ver R/ranking_universo.R), persistida en data/tac_manual.csv por nombre.
  tac_manual <- reactiveVal(cargar_tac_manual())

  tabla_con_tac <- reactive({
    d <- estado()
    req(d, nrow(d$tabla) > 0)
    tac <- tac_manual()
    d$tabla %>%
      dplyr::left_join(tac %>% dplyr::mutate(tac = suppressWarnings(as.numeric(tac))), by = "nombre") %>%
      # Sharpe simplificado (rf = 0): retorno de los ultimos 90D anualizado
      # sobre la vol. anualizada -- mismas dos columnas ya cacheadas por
      # calcular_fila_ranking(), no requiere volver a scrapear la CMF.
      dplyr::mutate(sharpe = ifelse(is.na(d90) | is.na(vol) | vol <= 0, NA_real_,
                                    ((1 + d90)^(365 / 90) - 1) / vol))
  })

  output$cards_ranking <- renderUI({
    tabla <- tryCatch(tabla_con_tac(), error = function(e) NULL)
    if (is.null(tabla)) return(NULL)
    albion <- tabla %>% dplyr::filter(nombre == "FI Albion") %>% dplyr::slice(1)
    if (!nrow(albion)) return(NULL)

    prom <- function(campo) mean(tabla[[campo]], na.rm = TRUE)
    delta <- function(campo) {
      v <- albion[[campo]][1]; p <- prom(campo)
      if (is.na(v) || is.na(p)) "vs promedio: s/d" else sprintf("%+.2f%% vs promedio", (v - p) * 100)
    }

    # ---- Ranking dinamico: sigue la columna/direccion por la que este
    # ordenada la tabla de abajo (DT manda el estado en input$tabla_resultado_state
    # apenas se hace click en un encabezado). Si no se ha reordenado (o se
    # ordeno por una columna no numerica, ej. Fondo/Serie), cae al default MTD.
    # El indice de columna es 0-based y debe calzar con el orden del transmute()
    # de output$tabla_resultado.
    COL_CAMPO <- c(`2` = "tac", `3` = "vc", `4` = "d1", `5` = "semana", `6` = "mtd",
                   `7` = "d30", `8` = "d90", `9` = "mes", `10` = "ytd", `11` = "vol",
                   `12` = "sharpe", `13` = "patrimonio", `14` = "aportantes", `15` = "atraso")
    COL_LABEL <- c(`2` = "TAC", `3` = "VC", `4` = "1D", `5` = "Semana", `6` = "MTD",
                   `7` = "30D", `8` = "90D", `9` = toupper(albion$mes_label[1] %||% "Mes"),
                   `10` = "YTD", `11` = "Vol. Anual", `12` = "Sharpe",
                   `13` = "Patrimonio", `14` = "Aportantes", `15` = "Atraso")

    campo_orden <- "mtd"; label_orden <- "MTD"; dir_orden <- "desc"
    orden <- input$tabla_resultado_state$order
    if (!is.null(orden) && length(orden) >= 1) {
      idx <- as.character(orden[[1]][[1]])
      if (idx %in% names(COL_CAMPO)) {
        campo_orden <- COL_CAMPO[[idx]]; label_orden <- COL_LABEL[[idx]]; dir_orden <- orden[[1]][[2]]
      }
    }

    vals_orden  <- suppressWarnings(as.numeric(tabla[[campo_orden]]))
    v_albion    <- suppressWarnings(as.numeric(albion[[campo_orden]][1]))
    n_rk        <- sum(!is.na(vals_orden))
    rank_val    <- if (is.na(v_albion)) NA_integer_ else if (identical(dir_orden, "asc"))
      sum(vals_orden < v_albion, na.rm = TRUE) + 1L else sum(vals_orden > v_albion, na.rm = TRUE) + 1L

    # Formato del "promedio universo" de la tarjeta de ranking: depende de que
    # tipo de columna se este mostrando (retorno/vol/sharpe/plata/dias/etc).
    .fmt_prom <- function(campo, v) {
      if (is.na(v)) return("s/d")
      if (campo %in% c("d1","semana","mtd","d30","d90","mes","ytd")) sprintf("%+.2f%%", v * 100)
      else if (campo == "vol") sprintf("%.2f%%", v * 100)
      else if (campo == "sharpe") sprintf("%.2f", v)
      else if (campo == "tac") sprintf("%.2f%%", v)
      else if (campo == "vc") formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
      else if (campo == "patrimonio") formatC(v, format = "f", digits = 0, big.mark = ".")
      else if (campo == "aportantes") formatC(v, format = "d", big.mark = ".")
      else if (campo == "atraso") sprintf("%.1f dias", v)
      else sprintf("%.2f", v)
    }

    .card <- function(titulo, valor, sub) div(class = "au-card",
      div(class = "au-card-title", titulo), div(class = "au-card-valor", valor), div(class = "au-card-sub", sub))

    div(class = "au-cards-row",
      .card(paste("Ranking", label_orden), if (is.na(rank_val)) "s/d" else sprintf("#%d de %d", rank_val, n_rk),
            paste("promedio universo", .fmt_prom(campo_orden, prom(campo_orden)))),
      .card("MTD Albion", .fmt_rent(albion$mtd[1]), delta("mtd")),
      .card("30D Albion", .fmt_rent(albion$d30[1]), delta("d30")),
      .card(paste(albion$mes_label[1], "Albion"), .fmt_rent(albion$mes[1]), delta("mes")),
      .card("YTD Albion", .fmt_rent(albion$ytd[1]), delta("ytd")),
      .card("Vol. Anualizada", ifelse(is.na(albion$vol[1]), "s/d", sprintf("%.2f%%", albion$vol[1] * 100)),
            sprintf("promedio universo %.2f%%", prom("vol") * 100)),
      .card("Sharpe Albion", ifelse(is.na(albion$sharpe[1]), "s/d", sprintf("%.2f", albion$sharpe[1])),
            paste("promedio universo", .fmt_prom("sharpe", prom("sharpe")))),
      .card("Serie de Albion comparada", albion$serie[1],
            ifelse(is.na(albion$tac[1]), "TAC s/d", sprintf("TAC %s%%", albion$tac[1])))
    )
  })

  output$tabla_resultado <- renderDT({
    tabla_full <- tryCatch(tabla_con_tac(), error = function(e) NULL)
    if (is.null(tabla_full)) return(DT::datatable(tibble(Aviso = "Sin datos todavia."), rownames = FALSE, options = list(dom = "t")))
    mes_lbl <- paste0(tabla_full$mes_label[1] %||% "MES", " %")

    tabla <- tabla_full %>%
      transmute(
        Fondo = ifelse(nombre == "FI Albion", paste0("★ ", nombre), nombre), Serie = serie,
        `TAC serie %` = ifelse(is.na(tac), "", as.character(tac)),
        VC = ifelse(is.na(vc), "-", formatC(vc, format = "f", digits = 2, big.mark = ".", decimal.mark = ",")),
        `1D %` = .fmt_rent_vec(d1), `Semana %` = .fmt_rent_vec(semana), `MTD %` = .fmt_rent_vec(mtd),
        `30D %` = .fmt_rent_vec(d30), `90D %` = .fmt_rent_vec(d90),
        !!mes_lbl := .fmt_rent_vec(mes),
        `YTD %` = .fmt_rent_vec(ytd),
        `Vol. Anual %` = ifelse(is.na(vol), "s/d", sprintf("%.2f%%", vol * 100)),
        # Sharpe queda NUMERICO (no string como el resto de las columnas
        # formateadas): con DT en modo server-side (default), ordenar una
        # columna de texto la ordena alfabeticamente (ej. "993.12" antes que
        # "9.58") en vez de numericamente -- confirmado en vivo, rompia el
        # ranking de la tarjeta de arriba vs. el orden visible de la tabla.
        # El formato a 2 decimales se aplica despues con DT::formatRound().
        Sharpe = sharpe,
        # format="d" desborda para Patrimonio (miles de millones, > 32 bits) y
        # devuelve "NA" -- confirmado en vivo. format="f" con digits=0 no tiene
        # ese limite porque no pasa por un entero de 32 bits internamente.
        Patrimonio = ifelse(is.na(patrimonio), "-", formatC(patrimonio, format = "f", digits = 0, big.mark = ".")),
        Aportantes = ifelse(is.na(aportantes), "-", formatC(aportantes, format = "d", big.mark = ".")),
        `Atraso (D)` = ifelse(is.na(atraso), "-", atraso)
      )
    idx_tac <- which(names(tabla) == "TAC serie %") - 1

    idx_mtd <- which(names(tabla) == "MTD %") - 1
    DT::datatable(tabla, rownames = FALSE, selection = list(mode = "multiple", target = "row"),
                  editable = list(target = "cell", disable = list(columns = setdiff(seq_along(tabla) - 1, idx_tac))),
                  # Orden inicial = MTD desc (mismo default que la tarjeta "Ranking MTD"
                  # de arriba); clickear otro encabezado reordena la tabla Y esa tarjeta.
                  # orderSequence "desc" en todas las columnas: cada clic vuelve a ordenar
                  # de mayor a menor (nunca de menor a mayor), para no generar confusion
                  # sobre si un ranking se esta leyendo al reves.
                  options = list(dom = "ft", pageLength = 50, order = list(list(idx_mtd, "desc")),
                                columnDefs = list(list(targets = "_all", orderSequence = list("desc")))),
                  class = "stripe hover") %>%
      DT::formatStyle("Fondo", target = "row",
        fontWeight = DT::styleEqual("★ FI Albion", "700"),
        backgroundColor = DT::styleEqual("★ FI Albion", "#e8edf5")) %>%
      DT::formatRound("Sharpe", digits = 2)
  })

  # "TAC serie %" es siempre la 3ra columna de la tabla (indice 2, 0-based --
  # ver el orden fijo del transmute() en tabla_resultado), la unica editable.
  observeEvent(input$tabla_resultado_cell_edit, {
    info <- input$tabla_resultado_cell_edit
    req(info$col == 2)
    tabla_full <- tryCatch(tabla_con_tac(), error = function(e) NULL)
    req(tabla_full)
    nombre_editado <- tabla_full$nombre[info$row]
    valor_nuevo <- gsub(",", ".", trimws(as.character(info$value)))
    tac <- tac_manual() %>% dplyr::filter(nombre != nombre_editado) %>%
      dplyr::bind_rows(tibble(nombre = nombre_editado, tac = valor_nuevo))
    tac_manual(tac)
    guardar_tac_manual(tac)
  })

  # ---- Ranking YTD -> Comparar Carteras: llevar la seleccion de filas a la
  # otra pestana y disparar la comparacion de una (misma logica que boton
  # "Comparar", ver .ejecutar_comparacion() mas arriba).
  observeEvent(input$btn_ir_comparar, {
    d <- estado()
    idx <- input$tabla_resultado_rows_selected
    if (is.null(d) || !nrow(d$tabla) || is.null(idx) || length(idx) == 0) {
      showNotification("Selecciona uno o mas fondos en la tabla primero.", type = "warning")
      return()
    }
    nombres_sel <- d$tabla$nombre[idx]
    updateSelectizeInput(session, "fondos_comparar", selected = nombres_sel)
    updateTabsetPanel(session, "main_tabs", selected = "Comparar Carteras")
    fondos_sel <- Filter(function(f) f$nombre %in% nombres_sel, FONDOS_UNIVERSO36)
    .ejecutar_comparacion(fondos_sel)
  })
}

shinyApp(ui, server)
