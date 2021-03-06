###########################################################################
#### Funci�n para generar un diagrama circular con porcentajes impresos ###
###########################################################################

## Argumentos de la funci�n: 2, uno obligatirio y otro opcional
## variable: Una variable categ�riga guardada en un objeto de clase "factor", es obligatorio
## titulo: Cadenas de caracteres. Es opcional, si no se proporcionan igual el c�digo corre. Son opcionales



########## inicio de funci�n ##################
diagrama.circular=function(variable,titulo=NULL){

variable=na.omit(variable) ### elimina posibles valores perdidos
#library(colorspace, pos=4) ### carga el paquete colorspace. Esta librer�a no es obligatoria

pie(table(variable), labels=paste(names(table(variable)),round(table(variable)*100/length(variable),2)," %"), xlab=NULL, ylab=NULL, main=titulo, 
col=rainbow(length(levels(variable))))

}
########## fin de funci�n ##################

ls() ## Comprueba que la funci�n est� en la memoria de R

class(diagrama.circular) ## Determinaci�n de la naturaleza del objeto 


######### EJERCICIO: Aplicar la funci�n class al conjunto de datos EPA y determine el tipo de objeto al que este corresponde


#### ejemplos: Requiere importar primero EPA y llamarlo del miso modo ######

## gr�fico sin t�tulos
diagrama.circular( EPA$ESTADO.CIVIL )

## Con t�tulo
diagrama.circular(EPA$ESTADO.CIVIL ,titulo="Distribuci�n porcentual de los estados civiles") 

######### EJERCICIO: Elaborar un diagrama circular con t�tulo para la variable COMUNIDAD AUT�NOMA 


















