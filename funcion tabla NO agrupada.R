### funci�n para generar tabla no agrupada
### Argumentos: 
### X: Vector num�rico o factor
### acumulado: especifica si se desea incluir frecuencias acumuladas, por defecto Verdadero
### deci: n�mero de lugares decimales para las frecuencias relativas (4 por defecto)

## Inicio de funci�n
tabla.A=function(X,acumulalado=T,deci=4){
options(warn=-1) # desactiva los warnings
X=na.omit(X)     # Elimina los valores perdidos 
x=names(table(sort(X))) # extrae las categor�as o valores de la variable
f=as.numeric(table(sort(X))) # c�culo de frecuencias absolutas
h=round(f/length(X),deci)    # c�culo de frecuencias relativas
if (acumulalado==T) {
  F=cumsum(as.numeric(table(sort(X)))) # c�culo de frecuencias absolutas acumuladas
  H=round(F/length(X),deci) # c�culo de frecuencias relativas acumuladas
  tablaA=data.frame(x,f,F,h,H) # generaci�n de la tabla
  tablaA[length(x)+1,]=c("",sum(f),"*",sum(h),"*") # adici�n de la fila con los totales
  levels(tablaA$x)[length(f)+1]="sumas" # Adici�n de la palabra suma a la �ltima fila
  tablaA$x[length(f)+1]="sumas" # Adici�n de la palabra suma a la �ltima fila
  }else{
  tablaA=data.frame(x,f,h)
  tablaA[length(x)+1,]=c("sumas",sum(f),sum(h)) 
  levels(tablaA$x)[length(f)+1]="sumas"
  tablaA$x[length(f)+1]="sumas"
}


return(tablaA)
options(warn=0)
}
## fin de funci�n

## EJEMPLOS: Primero se debe cargar el conjunto de datos EPA

# Con la variable estado civil, sin acumulados
X=EPA$ESTADO.CIVIL
tabla.A(X,F)

# Con la variable EDAD
X=EPA$EDAD
tabla.A(X)

# Con la variable ESTUDIOS, tres decimales de precisi�n
X=EPA$ESTUDIOS
tabla.A(X, deci=3)