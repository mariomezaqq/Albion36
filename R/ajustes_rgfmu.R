# =============================================================================
# AJUSTE POR FACTOR DE REPARTO (RGFMU)
# Extraido de Pulso-VCC/R/dividendos.R (misma logica, sin el resto del modulo
# de dividendos boletin/Excel que Pulso usa y este proyecto no necesita).
# =============================================================================

#' Agrega valor_cuota_ajustado = valor_cuota * cumprod(factor_reparto)
#' Dias sin reparto (factor NA o <= 0) se tratan como factor neutro 1.
#'
#' @param historico tibble(fecha, valor_cuota, factor_reparto)
#' @return el mismo tibble + columnas valor_cuota_ajustado, div_acum
aplicar_factor_reparto <- function(historico) {
  if (is.null(historico) || nrow(historico) == 0) return(historico)

  historico <- historico %>% dplyr::arrange(fecha)

  fr <- if ("factor_reparto" %in% colnames(historico)) as.numeric(historico$factor_reparto) else NA_real_
  fr <- rep_len(fr, nrow(historico))
  fr[is.na(fr) | fr <= 0] <- 1
  fa_acum <- cumprod(fr)

  historico$valor_cuota_ajustado <- historico$valor_cuota * fa_acum
  historico$div_acum <- historico$valor_cuota_ajustado - historico$valor_cuota
  historico
}
