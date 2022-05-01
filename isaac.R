library(lifecontingencies)
get_life_table<- function(tabla){
  if (tabla=="m"){
    table <- read_excel("Datos.xlsx", sheet = "Mlx")
    lifet<-probs2lifetable(table$px, radix = 1000000000000, type = "px", name="Tabla de Mujer" )
    lifet
    print('mujer')
  }
  if(tabla=="h"){
    table <- read_excel("Datos.xlsx", sheet = "Hlx")
    lifet<-probs2lifetable(table$px, radix = 1000000000000, type = "px", name="Tabla de Hombre" )
    print('hombre')
  }
    if(tabla=="inv"){
    table <- read_excel("Datos.xlsx", sheet = "qxinv")
    lifet<-probs2lifetable(table$px, radix = 1000000000000, type = "px", name="Tabla de Hombre" )
    print('hombre')
  }
  return(lifet)
}

sobrevivencia_conjuge <- function(fecha_hoy,
                                  semanas_cotizadas,
                                  carrera_salarial,
                                  datos,
                                  INPC,
                                  UDIs,
                                  tasas) {
  carrera_salarial=as.data.frame(carrera_salarial)
  carrera_salarial_ajustada=carrera_salarial$Salario_diario*
    (tasa_ajuste_salario$rate[0:length(carrera_salarial$Salario_diario)])

  tabla_asegurado=get_life_table(datos$Sexo[1])

  table_conyuge=get_life_table(datos$Sexo[2])
  #CALCULO b1 #
  Sal_promedio = sum(carrera_salarial_ajustada)/10
  CB_IV = .35 * Sal_promedio
  AF = .15
  AA = .135
  PGM = 107.8878642 #pension en el 2022
  CB_IV_AA_AF = CB_IV * (1 + AA + AF)
  maximo_interno_b1 = max(CB_IV_AA_AF, PGM)
  maximo_externo = max(CB_IV, PGM)
  b1 = (min(maximo_interno_b1, Sal_promedio) + maximo_externo)*365/12
  #CALCULO b2 #
  CB_IV_AF = CB_IV * (1 + AF)
  maximo_interno_b2 = max(CB_IV_AF, PGM)
  b2 = (min(maximo_interno_b2, Sal_promedio) + maximo_externo)*365/12
  i=tasas$rate[1]
  anualidad=-((1+i)^(-1)-1)/(1-(1+i)^(-1/12))
  edad_conyuge=as.integer(datos[2,1])
  edades_conyuge=(0:(110-edad_conyuge)+1)
  probs_conyuge=pxt(table_conyuge,edad_conyuge,edades_conyuge)
  convs_conyugue=probs_conyuge*b1+(1-probs_conyuge)*b2


  return(anualidad)
}