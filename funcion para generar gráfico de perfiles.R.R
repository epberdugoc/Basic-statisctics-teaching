
#####  Procedimiento para generar un gráfico de perfiles en R ##########
##### AUTOMATIZACIÓN DEL CÓDIGO MEDIANTE UNA FUNCIÓN ######


## argumentos ##########

## datos: Nombre del conjunto de datos a utilizar
## variable.fila: cadena de texto (entre comillas) que indica el nombre de la variable fila en el conjunto de datos.
## variable.columna: cadena de texto (entre comillas) que indica el nombre de la variable columna.
## tipo: cadena de texto, sólo puede ser "fila" o "columna"
## ubicacion.y: Coordenada "y" para la ubicación de la leyenda
## ubicacion.x: Coordenada "x" para la ubicación de la leyenda

###################################################
############ inicio de función ####################
###################################################

Grafico.Perfil=function(datos,variable.fila, variable.columna, tipo="fila", ubicacion.x=15.5, ubicacion.y=60){
  
require(Rcmdr);library(abind)
datos<- na.omit(datos)
attach(datos)
X=datos[,variable.fila]
Y=datos[,variable.columna]

  if(tipo=="fila")
{
  Tabla.conteo <- xtabs(~Y+X , data=datos) #Genera la tabla de contingencia  
  Tabla.perfil=colPercents(Tabla.conteo )[1:length(levels(Y)),1:length(levels(X))]
  op=par(mai=c(1,1,.5,2.5)) ## agranda el margen derecho para ubicar las convenciones de colores
  
  barplot(Tabla.perfil , xlab=colnames(datos)[variable.fila]  , ylab="Porcentaje",legend.text=T,beside=TRUE,col=cm.colors(length(levels(Y))), args.legend = 
            list(x = ubicacion.x, y=ubicacion.y, bty="n",title =variable.columna ), main=paste("Gráfico de Perfil","fila", collapse=" "))
  
  par(op) ## restituye los márgenes originales para un próximo gráfico
}
if(tipo=="columna")
{
Tabla.conteo <- xtabs(~X+Y , data=datos) #Genera la tabla de contingencia  
    Tabla.perfil=colPercents(Tabla.conteo )[1:length(levels(X)),1:length(levels(Y))]
    op=par(mai=c(1,1,.5,2.5)) ## agranda el margen derecho para ubicar las convenciones de colores
    
    barplot(Tabla.perfil , xlab=colnames(datos)[variable.columna], ylab="Porcentaje",legend.text=T,beside=TRUE,col=cm.colors(length(levels(X))), args.legend = 
              list(x = ubicacion.x, y=ubicacion.y, bty="n",title =variable.fila ), main="Gráfico de Perfil columna")
    
    par(op) ## restituye los márgenes originales para un próximo gráfico
}
detach(datos)
}
###################################################
########## fin de función ##################
###################################################

## ejemplo: perfil fila EPA ####
### Primero se debe importar el conjunto de datos EPA y llamrlo de la misma forma

Grafico.Perfil(datos=EPA,variable.fila="ESTADO.CIVIL",variable.columna="ESTUDIOS",tipo="fila",ubicacion.x=24,ubicacion.y=50)

## ejemplo: perfil columna EPA ####

Grafico.Perfil(datos=EPA,variable.fila="ESTADO.CIVIL",variable.columna="ESTUDIOS",tipo="columna",ubicacion.x=28,ubicacion.y=50)

## Ejercicio: Gráfico de perfil columna para las variables MADUREZ y SEXO del conjunto de datos Pescaos

Grafico.Perfil(datos=Pescaos,variable.fila="madurez",variable.columna="sexo",tipo="columna",ubicacion.x=28,ubicacion.y=50)







