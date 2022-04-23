#tarea larga2 seguridad social y pensiones

#preguntar la fecha de hoy
fecha_hoy<-Sys.Date()

#sexo del trabajador
sexo_trabajador<-readline(prompt = "¿Se identifica como hombre: h o mujer: m? ")

#edad del trabajador
edad_trabajador<-as.numeric(readline(prompt = "¿Cuántos años tiene?: "))

#semanas cotizadas del trabajador
semanas_cotizadas<-as.numeric(readline(prompt = "¿Cuántas semanas tiene cotizadas?: "))

#asegurarnos de saber si tiene derecho a pension
if(semanas_cotizadas>250){

  #salario promedio mensual de los ultimos 10 años
  salario_mensual<-c()
  for (i in 1:10) {
    salario_mensual[i]<-as.numeric(readline(prompt = paste("¿Cuál fue su salario mensual promedio en ",(as.numeric(format(fecha_hoy,'%Y'))-i+1),"? ",sep = "")))
  }

  #saber si el trabajador tiene conyuge
  conyuge<-readline(prompt = "¿Tiene conyuge? ")

  #edad y sexo de su conyuge en caso de tenerla
  if(conyuge=="si"){
    #sexo de su conyuge
    sexo_conyuge<-readline(prompt = "¿Su conyuge se identifica como hombre: H o mujer: M? ")

    #edad de su conyuge
    edad_conyuge<-as.numeric(readline(prompt = "¿Cuántos años tiene su conyuge?: "))
  }

  #numero de hijos del trabajador
  num_hijos<-as.numeric(readline(prompt = "¿Cuántos hijos tiene?: "))

  #edades y sexos de los hijos del trabajador en caso de tener, tambien saber si son invalidos
  edad_hijos<-c()
  sexo_hijos<-c()
  if(num_hijos != 0){
    for (i in 1:num_hijos) {
      #edad de los hijos
      edad_hijos[i]<-as.numeric(readline(prompt = paste("Edad del hijo ",i,": ",sep = "")))

      #sexo de los hijos
      sexo_hijos[i]<-readline(prompt = paste("¿Su hijo ", i," se identifica como hombre: h o mujer: m? ",sep = ""))

      #invalidez de los hijos
      invalidez_hijos[i]<-readline(prompt = paste("¿Su hijo ", i," se encuentra invalido? ",sep = ""))
    }
  }

  #dependencia economica del padre
  padre<-readline(prompt = "¿Su padre depende economicamente de usted? ")

  #edad del padre en caso de depender economicamente
  if(padre=="si"){
    #edad de su conyuge
    edad_padre<-as.numeric(readline(prompt = "¿Cuántos años tiene su padre?: "))
  }

  #dependencia economica de la madre
  padre<-readline(prompt = "¿Su madre depende economicamente de usted? ")

  #edad del padre en caso de depender economicamente
  if(madre=="si"){
    #edad de su conyuge
    edad_madre<-as.numeric(readline(prompt = "¿Cuántos años tiene su madre?: "))
  }

}else{readline("No tiene derecho a pensión")}

