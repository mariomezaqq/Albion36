# =============================================================================
# GRAFICOS DE COMPOSICION DE CARTERA (plotly) -- circulares (dona), uno por
# fondo (no comparan entre fondos): % Nacional/Extranjera, % por Tipo de
# Instrumento, % por Clase. Van debajo de la tabla de cada fondo.
#
# Se usa plotly (SVG/canvas renderizado en el navegador) en vez de ggplot2 +
# renderPlot (imagen PNG rasterizada en el servidor) porque esta ultima se ve
# pixelada con el escalado de pantalla de Windows -- plotly queda nitido a
# cualquier zoom/DPI y de paso trae tooltips interactivos gratis.
# =============================================================================
suppressMessages({ library(dplyr); library(plotly) })

# Paleta consistente con TEMA_CSS (acento navy + variaciones + pos/neg + gris)
.PALETA_GRAFICOS <- c("#1b2d5b", "#3d5a99", "#6f8fc7", "#a7bfe0", "#c8d5ec",
                       "#1a7f4b", "#5fa87e", "#c0392b", "#e0897c", "#7a869a")

#' Colapsa niveles poco frecuentes de una columna categorica en "Otros",
#' quedandose con los `top_n` de mayor pct.
.colapsar_otros <- function(datos, col, top_n = 6) {
  datos <- datos %>% arrange(desc(pct))
  if (nrow(datos) <= top_n) return(datos)
  top <- utils::head(datos[[col]], top_n)
  datos[[col]] <- ifelse(datos[[col]] %in% top, datos[[col]], "Otros")
  datos %>% group_by(across(-pct)) %>% summarise(pct = sum(pct), .groups = "drop")
}

#' Tibble: categoria_simple (Nacional/Extranjera/Otros), pct -- para UN fondo
agregar_categoria_fondo <- function(tabla_actual) {
  if (is.null(tabla_actual) || nrow(tabla_actual) == 0) return(NULL)
  tabla_actual %>%
    mutate(categoria_simple = ifelse(categoria %in% c("Nacional", "Extranjera"), categoria, "Otros")) %>%
    group_by(categoria_simple) %>%
    summarise(pct = sum(pct_activo_fondo, na.rm = TRUE), .groups = "drop") %>%
    filter(pct > 0)
}

#' Tibble: tipo_instrumento (top 6 + Otros), pct -- para UN fondo
agregar_tipo_fondo <- function(tabla_actual) {
  if (is.null(tabla_actual) || nrow(tabla_actual) == 0) return(NULL)
  datos <- tabla_actual %>%
    mutate(tipo_instrumento = ifelse(is.na(tipo_instrumento) | tipo_instrumento == "", "(s/d)", tipo_instrumento)) %>%
    group_by(tipo_instrumento) %>%
    summarise(pct = sum(pct_activo_fondo, na.rm = TRUE), .groups = "drop") %>%
    filter(pct > 0)
  .colapsar_otros(datos, "tipo_instrumento")
}

#' Tibble: clase, pct -- para UN fondo (periodo actual)
agregar_clase_fondo <- function(tabla_actual) {
  if (is.null(tabla_actual) || nrow(tabla_actual) == 0) return(NULL)
  tabla_actual %>%
    group_by(clase) %>%
    summarise(pct = sum(pct_activo_fondo, na.rm = TRUE), .groups = "drop") %>%
    filter(pct > 0)
}

#' Grafico circular (dona) chico, nitido a cualquier DPI, con tooltip.
grafico_torta <- function(datos, fill_col, titulo) {
  if (is.null(datos) || nrow(datos) == 0) return(NULL)
  datos <- datos %>% arrange(desc(pct))
  colores <- .PALETA_GRAFICOS[seq_len(nrow(datos))]

  plotly::plot_ly(
    datos, labels = as.formula(paste0("~", fill_col)), values = ~pct,
    type = "pie", hole = 0.5, sort = FALSE,
    marker = list(colors = colores, line = list(color = "#ffffff", width = 2)),
    textinfo = "percent", textposition = "inside", insidetextorientation = "horizontal",
    textfont = list(color = "#ffffff", size = 13, family = "Lato, sans-serif"),
    hovertemplate = "%{label}: %{percent}<extra></extra>"
  ) %>%
    plotly::layout(
      title = list(text = titulo, font = list(color = "#1b2d5b", size = 14, family = "Lato, sans-serif"),
                   x = 0.5, xanchor = "center"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05,
                    font = list(size = 10.5, family = "Lato, sans-serif", color = "#1a2233")),
      margin = list(t = 45, b = 10, l = 10, r = 10),
      paper_bgcolor = "transparent", plot_bgcolor = "transparent"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
