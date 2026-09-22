# =============================================================================
# TEMA VISUAL "VIZCAYA CAPITAL" (mismo look de la app en vivo albion-universo36
# y de Pulso-VCC/R/ui_dashboard.R -- mismos tokens de color/tipografia
# confirmados por inspeccion en vivo: --accent #1b2d5b, --bg #f0f2f5,
# fuentes Lato + Playfair Display).
# =============================================================================

TEMA_CSS <- paste0(
  "@import url('https://fonts.googleapis.com/css2?family=Lato:wght@300;400;700&family=Playfair+Display:wght@600&display=swap');",
  "*,*::before,*::after{box-sizing:border-box}",
  ":root{--bg:#f0f2f5;--surface:#ffffff;--surface2:#f7f8fa;--border:#dde1e8;",
  "--text:#1a2233;--muted:#7a869a;--accent:#1b2d5b;--accent-light:#e8edf5;",
  "--pos:#1a7f4b;--pos-bg:#eaf6f0;--neg:#c0392b;--neg-bg:#fdf0ef;--radius:8px}",
  "body{background:var(--bg);color:var(--text);font-family:'Lato',sans-serif;font-size:14px}",
  ".au-header{background:var(--surface);border-bottom:1px solid var(--border);",
  "margin:-15px -15px 20px;padding:18px 32px;display:flex;align-items:center;gap:20px;",
  "box-shadow:0 1px 4px rgba(0,0,0,.06)}",
  ".au-logo{height:44px;width:auto;flex-shrink:0}",
  ".au-header-divider{width:1px;height:36px;background:var(--border);flex-shrink:0}",
  ".au-brand{font-family:'Playfair Display',serif;font-size:18px;color:var(--accent);",
  "letter-spacing:.3px;font-weight:600}",
  ".au-subtitle{font-size:11px;color:var(--muted);margin-top:2px;text-transform:uppercase;letter-spacing:1.3px}",
  ".au-toolbar{margin-bottom:14px}",
  ".au-hint{font-size:12px;color:var(--muted);margin-bottom:14px}",
  ".au-divider{height:1px;background:var(--border);border:none;margin:18px 0}",
  ".au-table-wrap{background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);",
  "box-shadow:0 1px 4px rgba(0,0,0,.05);padding:4px;margin-bottom:10px;overflow-x:auto}",
  # ---- grid de carteras lado a lado: 3 tarjetas visibles por fila (ancho fijo,
  # flex-shrink:0 para que no se compriman con 4+ fondos). Sin wrap: el 4to
  # fondo y siguientes quedan a la derecha, fuera de pantalla -- se llega a
  # ellos con scroll horizontal de la PAGINA (no un scroll interno propio),
  # y cada tarjeta sigue creciendo a su alto natural para el scroll vertical
  # normal, como un fact sheet/PDF.
  ".au-cartera-grid{display:flex;flex-wrap:nowrap;gap:20px;align-items:flex-start}",
  ".au-cartera-item{flex:0 0 calc(33.333vw - 45px);max-width:calc(33.333vw - 45px);min-width:420px}",
  ".au-graficos-row{display:flex;gap:10px;margin:2px 0 10px}",
  ".au-graficos-row > div{flex:1 1 0;min-width:0}",
  ".au-graficos-row-1 > div{flex:0 1 60%;margin:0 auto}",
  ".au-fondo-title{font-family:'Playfair Display',serif;font-size:16px;color:var(--accent);",
  "margin:22px 0 4px;padding-bottom:8px;border-bottom:2px solid var(--accent-light)}",
  ".au-periodos{font-size:12px;color:var(--muted);margin-bottom:10px}",
  ".au-rentabilidades{display:flex;gap:16px;font-size:12.5px;color:var(--muted);",
  "margin:0 0 12px;padding:8px 12px;background:var(--surface2);border-radius:6px}",
  ".au-rentabilidades b{font-weight:700}",
  ".au-rent-item{display:flex;flex-direction:column;gap:2px}",
  ".au-rent-rank{font-size:10.5px;color:var(--muted)}",
  # ---- tarjetas resumen de Ranking YTD (Ranking MTD, MTD/30D/mes/YTD Albion,
  # Vol. Anualizada, Serie comparada) ----
  ".au-cards-row{display:flex;gap:12px;margin:14px 0 18px;flex-wrap:wrap}",
  ".au-card{flex:1 1 150px;min-width:150px;background:var(--surface);border:1px solid var(--border);",
  "border-radius:var(--radius);box-shadow:0 1px 4px rgba(0,0,0,.05);padding:12px 14px}",
  ".au-card-title{font-size:10.5px;color:var(--muted);text-transform:uppercase;letter-spacing:.5px;",
  "margin-bottom:6px}",
  ".au-card-valor{font-family:'Playfair Display',serif;font-size:22px;color:var(--accent);font-weight:600}",
  ".au-card-sub{font-size:11px;color:var(--muted);margin-top:4px}",
  # ---- botones ----
  ".btn-actualizar, #btn_actualizar, #btn_comparar{background:var(--accent)!important;color:#fff!important;",
  "border:none!important;border-radius:6px!important;padding:7px 16px!important;",
  "font-family:'Lato',sans-serif!important;font-size:12px!important;font-weight:700!important;",
  "letter-spacing:.5px;text-transform:uppercase;transition:background .15s}",
  "#btn_actualizar:hover, #btn_comparar:hover{background:#16244a!important}",
  # ---- pestanas (reskin de nav-pills de Shiny para que se vean como tab-bar) ----
  ".nav-pills{border-bottom:2px solid var(--border);gap:0;margin-bottom:20px}",
  ".nav-pills .nav-link{background:none!important;border:none!important;",
  "border-bottom:3px solid transparent!important;border-radius:0!important;",
  "padding:12px 18px!important;font-family:'Lato',sans-serif;font-size:12px;font-weight:700;",
  "color:var(--muted)!important;text-transform:uppercase;letter-spacing:.8px;margin-bottom:-2px}",
  ".nav-pills .nav-link:hover{color:var(--accent)!important;background:var(--accent-light)!important}",
  ".nav-pills .nav-link.active{color:var(--accent)!important;",
  "border-bottom-color:var(--accent)!important;background:var(--accent-light)!important}",
  # ---- tablas (aplica a las DT que generamos) ----
  ".dataTable thead th{background:var(--accent)!important;color:#fff!important;font-weight:700;",
  "font-size:11px;text-transform:uppercase;letter-spacing:.4px;padding:8px 9px!important;white-space:nowrap}",
  ".dataTable tbody tr{background:var(--surface)}",
  ".dataTable tbody tr.odd{background:var(--surface2)}",
  ".dataTable tbody tr:hover{background:var(--accent-light)!important}",
  ".dataTable tbody td{padding:6px 9px!important;font-size:12.5px;color:var(--text)}",
  ".au-cartera-item .dataTable{font-size:12.5px}",
  ".estado-nuevo{color:var(--pos);font-weight:700;background:var(--pos-bg);border-radius:4px;padding:2px 6px}",
  ".estado-salido{color:var(--neg);font-weight:700;background:var(--neg-bg);border-radius:4px;padding:2px 6px}",
  ".estado-subio{color:var(--pos);font-weight:700}",
  ".estado-bajo{color:var(--neg);font-weight:700}",
  ".estado-sincambio{color:var(--muted)}",
  ".alerta{margin:0 0 16px;padding:12px 18px;border-radius:var(--radius);font-size:13px;",
  "background:#fff8e1;border:1px solid #f0d27a;color:#7a5b00}"
)

#' Header con logo + marca, igual al de la app en vivo / Pulso-VCC.
au_header <- function(subtitulo) {
  shiny::tags$div(class = "au-header",
    if (file.exists("www/logo.jpg")) shiny::tags$img(class = "au-logo", src = "logo.jpg", alt = "Vizcaya Capital"),
    shiny::tags$div(class = "au-header-divider"),
    shiny::tags$div(
      shiny::tags$div(class = "au-brand", "Albion vs. Universo 36"),
      shiny::tags$div(class = "au-subtitle", subtitulo)
    )
  )
}
