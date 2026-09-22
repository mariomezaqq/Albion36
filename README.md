# Albion vs. Universo 36

Reconstrucción local del proyecto Shiny ya desplegado en
https://mariomezaqq.shinyapps.io/albion-universo36/ ("CFIALBIONA vs. universo
de fondos conservadores"). Ese deploy se hizo desde otro PC y **nunca se subió
a GitHub**, así que este esqueleto se reconstruyó desde cero reutilizando el
scraper CMF y la lógica de ajuste RGFMU de `Pulso-VCC` (misma cuenta, mismo
autor, mismo scraper — confirmado por el texto de la app en vivo).

## Qué falta para que funcione igual que el original

1. **Los 33 fondos conservadores.** Solo está cargado `FI Albion` (los datos
   CMF de ese fondo ya se conocían, vienen de Pulso-VCC). Los otros 33 fondos
   eran una lista "aportada por Mario" (una foto/captura, 07-ago-2026) que no
   existe en ningún repo — hay que completarla a mano en
   `R/fondos_universo36.R` siguiendo las instrucciones que están en ese
   archivo (buscar cada fondo en el buscador de la CMF, tomar la serie de
   **mayor TAC**, copiar `run`/`row` de la URL).
2. **Credenciales CMF.** Copia `R/credentials.example.R` como
   `R/credentials.R` y pega tu token reCAPTCHA (ver instrucciones en el
   archivo). En la nube se usa la variable de entorno `CMF_RECAPTCHA_TOKEN`
   en vez del archivo.

## Correr localmente

```r
shiny::runApp()
```

## Estructura

- `app.R` — UI + servidor. Botón "Actualizar universo" dispara el scrape
  secuencial de los N fondos y guarda `data/series_universo36.rds`.
- `R/scraper.R` — scraper CMF (copiado tal cual de Pulso-VCC).
- `R/ajustes_rgfmu.R` — ajuste de valor cuota por factor de reparto (RGFMU),
  copiado de la lógica de Pulso-VCC.
- `R/fondos_universo36.R` — catálogo de fondos a scrapear (**incompleto**,
  ver punto 1 arriba).
- `R/credenciales.R` / `R/credentials.example.R` — mismo mecanismo de
  credenciales que Pulso-VCC.
