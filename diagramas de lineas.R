
## Primero se debe importar EPA:

  variable=EPA$EDAD # Extrae la columna EDAD del conjunto de datos EPA

  tabla=table(variable)      ## crea una tabla de frecuancias para la variable EDAD
  x=as.integer(names(tabla)) ## Convierte en n�meros a las edades que la tabla tiene como nombres
  y=as.numeric(tabla)/97     ## extrae las frecuencias absolutas y las convierte en relativas
  plot(x,y, type="h")        ## Genra el gr�fico de l�neas
  

