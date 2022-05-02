library(lifecontingencies)
library(rlist)
#la incapacidad se considera 15%
get_life_table<- function(tabla){
  if (tabla=="m"){
    table <- read_excel("Datos.xlsx", sheet = "Mlx")
    lifet<-probs2lifetable(table$qx, radix = 1000000000000, type = "qx", name="Tabla de Mujer" )
    lifet
    print('mujer')
  }
  if(tabla=="h"){
    table <- read_excel("Datos.xlsx", sheet = "Hlx")
    lifet<-probs2lifetable(table$qx, radix = 1000000000000, type = "qx", name="Tabla de Hombre" )
    print('hombre')
  }
    if(tabla=="inv"){
    table <- read_excel("Datos.xlsx", sheet = "qxinv")
    lifet<-probs2lifetable(table$qx, radix = 1000000000000, type = "qx", name="Tabla de Hombre" )
    print('inv')
  }
  return(lifet)
}

invalidez_conjuge <- function(fecha_hoy,
                                  carrera_salarial,
                                  datos,
                                  UDIs,
                                  tasas) {
  tasa_seguridad_social=.035
  carrera_salarial=as.data.frame(carrera_salarial)
  carrera_salarial_ajustada=carrera_salarial$Salario_diario*
    (tasa_ajuste_salario$rate[0:length(carrera_salarial$Salario_diario)])

  tabla_asegurado=get_life_table('inv')

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
  edad_asegurado=as.integer(datos[1,1])

  edades_conyuge=(0:(110-edad_conyuge+1))
  probs_conyuge=pxt(table_conyuge,edad_conyuge,edades_conyuge)
  # pxt(table_conyuge,edad_conyuge,50)
  convs_conyugue=probs_conyuge*b1+(1-probs_conyuge)*b2
  edades_asegurado=(0:(110-edad_asegurado))
  prob_asegurado=pxt(tabla_asegurado,edad_asegurado,edades_asegurado)
  tasas_de_asegurado=(1+tasa_seguridad_social)^(-edades_asegurado)
  convs_asegurado=prob_asegurado*tasas_de_asegurado
  if(length(convs_conyugue)>length(convs_asegurado)){

    convs_asegurado[(length(convs_asegurado)+1):length(convs_conyugue)]=0


  }else if(length(convs_conyugue)<length(convs_asegurado)){

    convs_conyugue[(length(convs_conyugue)+1):length(convs_asegurado)]=0

  }
  fecha_hoy

  date=as.POSIXlt(fecha_hoy)
  date$mday=1


  index=which(UDIs$Fecha==(date))
  inicio_mes=UDIs$SP68257[index]
  inicio_mes


  inicio_ano=7.108233
  PNSI=anualidad*sum(convs_asegurado*convs_conyugue)

  FACBI=inicio_mes/inicio_ano
  return(PNSI*FACBI*1.03)#de comisones y adquisicion se asumen 3% extra
}
supervivencia_conjuge <- function(fecha_hoy,
                                  carrera_salarial,
                                  datos,
                                  UDIs
                                  ) {
  tasa_seguridad_social=.035
  # edades_asegurado=(0:(110-edad_asegurado))

  carrera_salarial=as.data.frame(carrera_salarial)
  carrera_salarial_ajustada=carrera_salarial$Salario_diario*
    (tasa_ajuste_salario$rate[0:length(carrera_salarial$Salario_diario)])

  tabla_asegurado=get_life_table('inv')
  cat('tabla conyuge')
  table_conyuge=get_life_table(datos$Sexo[2])
  cat('tabla conyuge')
  #CALCULO b1 #
  Sal_promedio = sum(carrera_salarial_ajustada)/10
  CB_IV = .35 * Sal_promedio
  AF = .15
  AA = .135
  PGM = 107.8878642 #pension en el 2022
  b1=.9
  CB_IV_AA_AF = CB_IV * (1 + AA + AF)
  rvda = CB_IV_AA_AF*b1
  edad_conyuge=as.integer(datos[2,1])
  edad_asegurado=as.integer(datos[1,1])
  edades_asegurado=(0:(110-edad_asegurado))
  tasas_de_asegurado=(1+tasa_seguridad_social)^(-edades_asegurado)
  edades_conyuge=(0:(110-edad_conyuge+1))
  convs_conyugue=pxt(table_conyuge,edad_conyuge,edades_conyuge)
  probs_asegurado=pxt(tabla_asegurado,edad_conyuge,edades_asegurado)
  convs_asegurado=(1-probs_asegurado)*tasas_de_asegurado
  if(length(convs_conyugue)>length(convs_asegurado)){

    convs_asegurado[(length(convs_asegurado)+1):length(convs_conyugue)]=0


  }else if(length(convs_conyugue)<length(convs_asegurado)){

    convs_conyugue[(length(convs_conyugue)+1):length(convs_asegurado)]=0

  }
  Suma_basica=sum(convs_asegurado*convs_conyugue)*b1*13
  maximo_externo = max(CB_IV, PGM)
  if (datos$Sexo[2]=='m' | rvda <=PGM ){
    Suma_final=Suma_basica*1.15
  }else{
   Suma_final=Suma_basica
  }




  date=as.POSIXlt(fecha_hoy)
  date$mday=1


  index=which(UDIs$Fecha==(date))
  inicio_mes=UDIs$SP68257[index]
  inicio_mes


  inicio_ano=7.108233


  FACBI=inicio_mes/inicio_ano
  return(maximo_externo*Suma_final*FACBI*(1+.02))#solo se toma en cuenta margen
}
monto_comulativo<-function (fecha_hoy,
                      carrera_salarial,
                      datos,
                      UDIs
                      ) {
  supervivencia=supervivencia_conjuge(fecha_hoy,
                                carrera_salarial,
                                datos,
                                UDIs
          )
  invalidez=invalidez_conjuge(fecha_hoy,
                              carrera_salarial,
                              datos,
                              UDIs,
                              tasas)
  return(supervivencia+invalidez)
}
