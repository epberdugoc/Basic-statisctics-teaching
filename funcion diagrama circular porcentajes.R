###########################################################################
#### Función para generar un diagrama circular con porcentajes impresos ###
###########################################################################

## Argumentos de la función: 2, uno obligatirio y otro opcional
## variable: Una variable categóriga guardada en un objeto de clase "factor", es obligatorio
## titulo: Cadenas de caracteres. Es opcional, si no se proporcionan igual el código corre. Son opcionales



########## inicio de función ##################
diagrama.circular=function(variable,titulo=NULL){

variable=na.omit(variable) ### elimina posibles valores perdidos
#library(colorspace, pos=4) ### carga el paquete colorspace. Esta librería no es obligatoria

pie(table(variable), labels=paste(names(table(variable)),round(table(variable)*100/length(variable),2)," %"), xlab=NULL, ylab=NULL, main=titulo, 
col=rainbow(length(levels(variable))))

}
########## fin de función ##################

ls() ## Comprueba que la función esté en la memoria de R

class(diagrama.circular) ## Determinación de la naturaleza del objeto 


######### EJERCICIO: Aplicar la función class al conjunto de datos EPA y determine el tipo de objeto al que este corresponde


#### ejemplos: Requiere importar primero EPA y llamarlo del miso modo ######

## gráfico sin títulos
diagrama.circular( EPA$ESTADO.CIVIL )

## Con título
diagrama.circular(EPA$ESTADO.CIVIL ,titulo="Distribución porcentual de los estados civiles") 

######### EJERCICIO: Elaborar un diagrama circular con título para la variable COMUNIDAD AUTÓNOMA 


















