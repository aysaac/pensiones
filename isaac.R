library(lifecontingencies)
sobrevivencia_conjuge <- function(fecha_hoy,
                                  semanas_cotizadas,
                                  carrera_salarial,
                                  datos,
                                  INPC,
                                  UDIs,
                                  tasas) {
  #CALCULO b1 #
  Sal_promedio = mean(carrera_salarial)
  CB_IV = .35 * Sal_promedio
  AF = .15
  AA = .135
  PGM = 107.8878642 #pension en el 2022
  CB_IV_AA_AF = CB_IV * (1 + AA + AF)
  maximo_interno_b1 = max(CB_IV_AA_AF, PGM)
  maximo_externo = max(CB_IV, PGM)
  b1 = min(maximo_interno_b1, Sal_promedio) + maximo_externo
  #CALCULO b2 #
  CB_IV_AF = CB_IV * (1 + AF)
  maximo_interno_b2 = max(CB_IV_AF, PGM)
  b2 = min(maximo_interno_b2, Sal_promedio) + maximo_externo
  i=tasas[1,'rate']
  anualidad=annuity(i,)
}