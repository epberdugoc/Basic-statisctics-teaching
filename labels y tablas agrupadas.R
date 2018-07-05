###############################################################
########### Asignación de labels con el paquete Hmisc #########
###############################################################

###### Cargar EPA primero

fix(EPA) ### Abre hoja de cálculo para editar la base 

######## EJERCICIO: Cambiar nombre de CCAA por Comunidad.Autonoma


summary(EPA)  ### Descripción INCOMPLETA de las variables el conjunto de datos (También disponible desde el Commader)

library(Hmisc) ### caraga el paquete Hmisc

describe(EPA)  ### Descripción de las variables el conjunto de datos. No funciona si no he cargado Hmisc previamente

label(EPA$Comunidad.Autonoma)="Comunidad autónoma a la que pertenece el encuestado" ### Asigna la etiqueta a la variable CCAA del dataset EPA (importado previamente)

label(EPA$EDAD)="Edad medida en años cumplidos" ### Asigna la etiqueta a la variable EDAD del dataset EPA

label(EPA$ESTADO.CIVIL )="Estado civil: Variable NOMINAL con 4 categorías" ### Asigna la etiqueta a la variable ESTADO CIVIL del dataset EPA

label(EPA$ESTUDIOS)="variable ordinal. Nivel de formación académico más alto" ### Asigna la etiqueta a la variable ESTUDIOS del dataset EPA

label(EPA$TIEMPO)="Número de semanas laboradas en el empleo actual. 0 significa desempleado" ### Asigna la etiqueta a la variable TIEMPO del dataset EPA

describe(EPA)  ### Ahora la descripción incluye las etiquetas para cada variable


######## EJERCICIO: Para el conjunto de datos PECES asiganar labels a cada una de las variables


###############################################################
########### Tablas agrupadas con el paquete agricolae  ########
###############################################################
X=EPA$TIEMPO
X=round(rgamma(100,shape = 1),3)
X=na.omit(X)
n=length(X)
delta=3
R=max(X)-min(X)+10^(-delta)

###### OPCIONAL: Función para generar los límites de clase (inconclusa)
#
limits=function(X,regla=c("Sturges","raiz","Scott","Diaconis") , delta=0,Lo=min(X)){
if ((regla!="Diaconis")&(regla!="Scott")&(regla!="Sturges")&(regla!="raiz")) {
  print("Nombre de regla equivocado")
}else{
  X=na.omit(X)
  n=length(X)
  R=max(X)-min(X)+10^(-delta)
  if (regla=="Sturges") {
    Nc=round(1+log2(n),0)
    w= ceiling(R/Nc*10^(delta))/10^(delta)
  }
  if (regla=="raiz") {
    Nc=round(sqrt(n),0)
    w= ceiling(R/Nc*10^(delta))/10^(delta)
  }
  if (regla=="Scott"){
    w=ceiling(3.5*sd(X)*(n^(-1/3))*10^(delta))/10^(delta)
    Nc=ceiling(R/w)
  }
  if (regla=="Diaconis"){
    w=ceiling(2*IQR(X)*(n^(-1/3))*10^(delta))/10^(delta)
    Nc=ceiling(R/w)
  }
  
  limites=seq(from=Lo,by=w,length.out = Nc+1) 
  return(list("n"=n,"R"=R,"w"=w,"Nc"=Nc,"limts"=limites)) 
}
  

}

######### FIN DE FUNCIÓN ###########################

## carga el paquete agricolae, el cual fue instalado previamente
library(agricolae) 

## Elabora un histograma. También se puede con el Commander
hist(EPA$TIEMPO) 

## imprime la información con la que fue construida el histograma
print(hist(EPA$TIEMPO)) 

## Otra forma de elaborar un histograma con el paquete agricolae.
graph.freq(EPA$TIEMPO,frequency=2,xlab="Tiempo (semanas)", ylab="Porcentaje de personas", main="Distribucipon de la variable tiempo")  


## imprime la tabla asociada al primer histograma
print(table.freq(hist(EPA$TIEMPO))) 

windows()
## imprime la tabla asociada al segundo histograma
print(table.freq(graph.freq(EPA$TIEMPO))) 

## aplica la regla de Sturges a los datos de la variable TIEMPO y devuelve la información usada para la agrupación
sturges.freq(EPA$TIEMPO) 

############### Modificación de los límites de la tabla #######################

(limites=c(0,90,180,270,360,450, 540,630,720)) ## crea el vector con los límites de los intervalos

print(table.freq(with(EPA, hist(TIEMPO, scale="percent",right = FALSE, breaks=limites, col="darkgray")))) ## imprime la tabla y el histograma


limites2=c(0,90,180,270,360,630,720) ### fusionando los intervalos 5, 6 y 7


print(table.freq(with(EPA, hist(TIEMPO, scale="percent", breaks=limites2, col="darkgray")))) ## imprime la tabla y el histograma con los intervalos fusionados


(limites=c(0,90,180,270,360, 540,720)) ### fusionando los intervalos 5 con 6 y 7 con 8


print(table.freq(with(EPA, hist(TIEMPO, scale="percent",right = FALSE, breaks=limites, col="darkgray")))) ## imprime la tabla y el histograma










