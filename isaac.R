sobrevivencia_conjuge<-function(fecha_hoy,
                                semanas_cotizadas,
                                carrera_salarial,
                                datos,
                                INPC,
                                UDIs){
  Sal_promedio=mean(carrera_salarial)
  Cuantia_basica=.35*Sal_promedio
  AF=.15
  AA=.135
  Cuantia_basica=Cuantia_basica*(1+AA+AF)

}