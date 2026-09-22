# =============================================================================
# UNIVERSO DE FONDOS CONSERVADORES vs. FI ALBION
# Catalogo de fondos a scrapear en la CMF. Mismo formato que Pulso-VCC/R/fondos_pulso.R
# (run, serie, row, tipoentidad) porque usa el mismo scraper (R/scraper.R).
#
# Regla del proyecto (segun la app ya desplegada en shinyapps.io):
#   - 33 fondos mutuos conservadores de administradoras chilenas + FI Albion = 34.
#   - De CADA fondo se usa SIEMPRE la serie de MAYOR TAC ("la mas cara"), tal
#     como aparece en el buscador de fondos de la CMF.
#   - Valor cuota se ajusta por factor de reparto (RGFMU) via aplicar_factor_reparto()
#     en R/ajustes_rgfmu.R.
#
# Lista pegada por el usuario (26-ago-2026) desde el ranking de la app en vivo --
# nombre + serie de mayor TAC ya resueltos ahi. El "run" (RUT del fondo) se
# resolvio cruzando esos nombres contra el directorio masivo de la CMF
# (institucional/seil/certificacion_cir1835_fmutuos.php, via
# R/directorio_fondos.R::obtener_directorio_cfm()) -- 27 de 32 calzaron exacto
# por nombre+serie; los 5 restantes (BancoEstado Mi Futuro x2, Coopeuch Compass,
# Conservador Focus, BCI Mach) no estaban en ese directorio y se buscaron a mano
# en el buscador de entidades de la CMF (consulta_busqueda.php).
#
# "row" queda vacio a proposito: se confirmo en vivo que el scraper (R/scraper.R,
# R/cartera_cmf.R) funciona igual sin ese parametro, con solo rut+tipoentidad.
# =============================================================================

FONDOS_UNIVERSO36 <- list(
  list(nombre = "FI Albion", run = "10757", serie = "A", row = "AABbsrAAjAAAAFyAAF", tipoentidad = "FINRE"),
  list(nombre = "FONDO MUTUO ITAU CARTERA CRECIMIENTO DEFENSIVO", run = "10021", serie = "SIMPLE", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BANCHILE CONSERVADOR", run = "10059", serie = "DIGITAL", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO SURA MULTIACTIVO PRUDENTE", run = "8773", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BANCOESTADO PERFIL E", run = "8846", serie = "CLASI", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO SANTANDER PRIVATE BANKING PRUDENTE", run = "8910", serie = "GLOBAL", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO FINTUAL CONSERVATIVE CLOONEY", run = "9568", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO PRINCIPAL GESTION ACTIVA CONSERVADOR", run = "9595", serie = "GLB", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO LARRAINVIAL PROTECCION", run = "8788", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO SANTANDER GESTION ACTIVA PRUDENTE", run = "9649", serie = "GLOBAL", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO SCOTIA PORTAFOLIO CONSERVADOR", run = "8886", serie = "CLASICA", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BCI CARTERA DINAMICA CONSERVADORA", run = "8638", serie = "CLASI", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BANCHILE PORTAFOLIO CONSERVADOR LARGO PLAZO", run = "8377", serie = "L", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO FONDO ACTIVO 2035", run = "9608", serie = "B", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO CREDICORP CAPITAL PROTEGE", run = "10138", serie = "TYBA", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO SANTANDER RENTA EXTRA LARGO PLAZO UF", run = "8387", serie = "UNIVE", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO SECURITY CONSERVADOR ESTRATEGICO", run = "8306", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO PRINCIPAL GESTION ACTIVA MUY CONSERVADOR", run = "9596", serie = "GLB", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BCI CARTERA PATRIMONIAL CONSERVADORA", run = "9063", serie = "INVER", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO LARRAINVIAL CUENTA ACTIVA CONSERVADORA", run = "9192", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BICE DIGITAL CONSERVADOR", run = "10050", serie = "DIGITAL", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO ITAU GESTIONADO CONSERVADOR", run = "8994", serie = "F1", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BANCOESTADO MI FUTURO CONSERVADOR", run = "9766", serie = "CLASICO", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO LARRAINVIAL AHORRO ACTIVO", run = "10260", serie = "D", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BICE CONSERVADOR", run = "8295", serie = "LIQUIDEZ", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO CONSORCIO DINAMICO CONSERVADOR", run = "9430", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BTG PACTUAL GESTION CONSERVADORA", run = "9872", serie = "I", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO ZURICH PERFIL CONSERVADOR", run = "9575", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BANCOESTADO MI FUTURO ACCESIBLE", run = "9765", serie = "CLASICO", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO COOPEUCH COMPASS CONSERVADOR", run = "10208", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO FINTUAL VERY CONSERVATIVE STREEP", run = "9730", serie = "A", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO CONSERVADOR FOCUS", run = "9810", serie = "B", row = "", tipoentidad = "RGFMU"),
  list(nombre = "FONDO MUTUO BCI MACH", run = "10358", serie = "DIGITAL", row = "", tipoentidad = "RGFMU")
)
