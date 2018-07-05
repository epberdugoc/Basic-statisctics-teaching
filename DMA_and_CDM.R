### Función para calcular la DMA y CDR #########################

### Argumentos: Uno sólo, consistente en la variable numércica 

########## inicio de función ##################
DMA=function(variable){
x=na.omit(variable) ### elimina posibles valores perdidos  
if(is.numeric(x)==T){
  
mediana=median(x) ### calcula la mediana de la variable
DMA=sum(abs(x-mediana))/(length(x)-1)
CDR=100*DMA/abs(mediana)
resultados=c(mediana,DMA,CDR)
names(resultados)=c("mediana","DMA","CDR(%)")
return(resultados)
} else  {
 print("ERROR: La variable NO es numérica") 
  
}
}
########## fin de función ##################

class(DMA)

### ejemplos: con EPA  ########

DMA(EPA$TIEMPO)

DMA(EPA$EDAD)

DMA(EPA$ESTADO.CIVIL) ## Mensaje de error

## Análisis grupal por nivel de estudio

variable=EPA$TIEMPO
grupos=EPA$ESTUDIOS

by(variable,grupos,DMA)


## Otra forma:

by(EPA$TIEMPO,EPA$ESTUDIOS,DMA)

## EJERCICIO: Comparar el grado de dispersión de la edad según
## los niveles de estudio




