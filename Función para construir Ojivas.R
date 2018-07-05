### FUNCIÓN PARA GRAFICAR UNA OJIVA MUESTRAL #####
### requiere el paquete Rcmdr ####

ojiva=function(variable){
require(Rcmdr)
histograma=hist(variable,plot=F)
Rango=max(histograma$breaks)-min(histograma$breaks)
FRA=cumsum(c(0,histograma$counts)/sum(histograma$counts))
plot(histograma$breaks,FRA,type="o",ylab="Frecuencia Realativa Acumulada",
xlab="Límites de la variable",tck=1,main="Ojiva",xaxs="i", lwd=1.8,col="blue",pch=1,
xaxp=c(min(histograma$breaks),max(histograma$breaks),5),yaxp=c(0,1,4))
abline(h=seq(0,1,by=0.05),v=seq(min(histograma$breaks),max(histograma$breaks),by=Rango/50))
}
 set.seed(111);ojiva(rnorm(1000)) ## un ejemplo con datos simulados ##
 
 ## Ejemplo presentacion power point
 
 nhijos=rep(0:7,c(419,255,375,215,127,54,24,23))
 windows()
 ojiva(nhijos)
 
 plot()

