### función para generar tabla no agrupada
### Argumentos: 
### X: Vector numérico o factor
### acumulado: especifica si se desea incluir frecuencias acumuladas, por defecto Verdadero
### deci: número de lugares decimales para las frecuencias relativas (4 por defecto)

## Inicio de función
tabla.A=function(X,acumulalado=T,deci=4){
options(warn=-1) # desactiva los warnings
X=na.omit(X)     # Elimina los valores perdidos 
x=names(table(sort(X))) # extrae las categorías o valores de la variable
f=as.numeric(table(sort(X))) # cáculo de frecuencias absolutas
h=round(f/length(X),deci)    # cáculo de frecuencias relativas
if (acumulalado==T) {
  F=cumsum(as.numeric(table(sort(X)))) # cáculo de frecuencias absolutas acumuladas
  H=round(F/length(X),deci) # cáculo de frecuencias relativas acumuladas
  tablaA=data.frame(x,f,F,h,H) # generación de la tabla
  tablaA[length(x)+1,]=c("",sum(f),"*",sum(h),"*") # adición de la fila con los totales
  levels(tablaA$x)[length(f)+1]="sumas" # Adición de la palabra suma a la última fila
  tablaA$x[length(f)+1]="sumas" # Adición de la palabra suma a la última fila
  }else{
  tablaA=data.frame(x,f,h)
  tablaA[length(x)+1,]=c("sumas",sum(f),sum(h)) 
  levels(tablaA$x)[length(f)+1]="sumas"
  tablaA$x[length(f)+1]="sumas"
}


return(tablaA)
options(warn=0)
}
## fin de función

## EJEMPLOS: Primero se debe cargar el conjunto de datos EPA

# Con la variable estado civil, sin acumulados
X=EPA$ESTADO.CIVIL
tabla.A(X,F)

# Con la variable EDAD
X=EPA$EDAD
tabla.A(X)

# Con la variable ESTUDIOS, tres decimales de precisión
X=EPA$ESTUDIOS
tabla.A(X, deci=3)