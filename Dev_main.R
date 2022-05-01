#tarea larga2 seguridad social y pensiones

##importar librerias
library(readxl)


#hacer dataframe con INPC
INPC <- read_excel("Datos.xlsx", sheet = "INPC")
#hacer dataframe con UDIs
UDIs<-  read_excel("Datos.xlsx", sheet = "UDIs")
#hacer dataframe con Desercion Escolar
Desercion_Escolar<- read_excel("Datos.xlsx", sheet = "Desercion escolar")
tasas<- read_excel("Datos.xlsx", sheet = "Tasas")

#preguntar la fecha de hoy
# fecha_hoy<-Sys.Date()
fecha_hoy<-as.Date("2022-3-18")

#sexo del trabajador
cat("¿Se identifica como hombre: h o mujer: m? ")
sexo_trabajador<-readline()

#edad del trabajador
cat("¿Cuántos años tiene?: ")

edad_trabajador<-as.numeric(readline())

#semanas cotizadas del trabajador.
cat("¿Cuántas semanas tiene cotizadas?: ")
semanas_cotizadas<-as.numeric(readline( ))

#asegurarnos de saber si tiene derecho a pension
if(semanas_cotizadas>250){

  #salario promedio diario de los ultimos 10 años trabajados, si no trabajó un año se agrega otro más
  Salario_diario<-c()
  ano<-c()
  cc<-10
  k<-1

  while (cc!=0) {
    cat(paste("¿Cuál fue su salario diario en ",(as.numeric(format(fecha_hoy,'%Y'))-k+1),", (si no tuvo poner 0)? "),sep = "")
    Salario_diario[k]<-as.numeric(readline())
    ano[k]<-(as.numeric(format(fecha_hoy,'%Y'))-k+1)
    if(Salario_diario[k]==0){
      cc<-cc+1
    }
    cc<-cc-1
    k<-k+1
  }
  carrera_salarial<-cbind(ano,Salario_diario)

  #saber si el trabajador tiene conyuge
  cat("¿Tiene conyuge (poner si o no)? ")
  conyuge<-readline( )

  #edad y sexo de su conyuge en caso de tenerla
  if(conyuge=="si"){
    #sexo de su conyuge
    cat("¿Su conyuge se identifica como hombre: h o mujer: m? ")
    sexo_conyuge<-readline()

    #edad de su conyuge
    cat("¿Cuántos años tiene su conyuge?: ")
    edad_conyuge<-as.numeric(readline())
  }else{
    sexo_conyuge<-"NA"
    edad_conyuge<-"NA"
  }

  #numero de hijos del trabajador
  cat("¿Cuántos hijos tiene (poner 0 si no tiene)?: \n")
  num_hijos<-as.numeric(readline())

  #edades y sexos de los hijos del trabajador en caso de tener, tambien saber si son invalidos
  edad_hijos<-c()
  edad_hijos[1]<-"NA"
  sexo_hijos<-c()
  sexo_hijos[1]<-"NA"
  invalidez_hijos<-c()
  invalidez_hijos[1]<-"NA"
  if(num_hijos != 0){
    for (i in 1:num_hijos) {
      #sexo de los hijos
      cat (paste("¿Su hijo ", i," se identifica como hombre: h o mujer: m? ",sep = ""))
      sexo_hijos[i]<-readline()
      #edad de los hijos
      cat( paste("Edad del hijo ",i,": ",sep = ""))
      edad_hijos[i]<-as.numeric(readline())
      #invalidez de los hijos
      cat("¿Su hijo ", i," se encuentra invalido? ",sep = "")
      invalidez_hijos[i]<-readline()
    }
  }

  if(num_hijos==0 && conyuge=="no"){
    #dependencia economica del padre
    cat("¿Su padre depende economicamente de usted (poner si o no)? ")
    padre<-readline()

    #edad del padre en caso de depender economicamente
    if(padre=="si"){
      #edad del padre
      cat("¿Cuántos años tiene su padre?: ")
      edad_padre<-as.numeric(readline())
      #sexo del padre
      sexo_padre<-"h"
    }else{
      edad_padre<-"NA"
      sexo_padre<-"NA"
      }

    #dependencia economica de la madre
    cat("¿Su madre depende economicamente de usted (poner si o no)? ")
    madre<-readline()

    #edad del padre en caso de depender economicamente
    if(madre=="si"){
      #edad de la madre
      cat("¿Cuántos años tiene su madre?: ")
      edad_madre<-as.numeric(readline())
      #sexo de la madre
      sexo_madre<-"m"
    }else{
      edad_madre<-"NA"
      sexo_madre<-"NA"
      }
  } else{
      edad_padre<-"NA"
      sexo_padre<-"NA"
      edad_madre<-"NA"
      sexo_madre<-"NA"
  }

  #creamos dataframe con la edad y sexo del trabajador, conyuge, hijos y padres
  datos<- data.frame(
    "Edad" = c(edad_trabajador, edad_conyuge, edad_hijos, edad_padre, edad_madre),
    "Sexo" = c(sexo_trabajador, sexo_conyuge, sexo_hijos, sexo_padre, sexo_madre),
    "Invalido" = c("NA", "NA", invalidez_hijos, "NA", "NA")
  )

  #creamos un vector con el nombre de los hijos para ponerlos en el dataframe de datos
  nombres_hijos<-c()
  for (i in 1:length(edad_hijos)) {
    nombres_hijos[i]<-paste("Hijo",i)
  }

  #ponemos los nombres de los familiares en el dataframe de datos
  row.names(datos)<-c("Trabajador", "Conyuge", nombres_hijos, "Padre", "Madre")



}else{cat("No tiene derecho a pensión")}

#las variables que necesitan para sus funciones
fecha_hoy
semanas_cotizadas
carrera_salarial
datos
INPC
UDIs
Desercion_Escolar




















