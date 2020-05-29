
#### Function to simulate a histogram as a uniform distributions mixture ######
## ARGUMENTS:
## breaks: A vector with the limits of the histogram intervals 
##     rf: A vector with the relative frequencies for each interval
##      n: The size of the simulated sample
##   seed: The seed used for the simulation (OPTIONAL)
## The outcome is a vector with the simulated values. It could be stored in a variable


hist.sample=function(breaks,rf,n, seed=NULL){
  if(is.null(seed)){
    muestra.intervalos=sample(1:length(rf), replace=T, prob=rf,size = n) 
    tabla=table(muestra.intervalos)
    c=unlist(tabla)
    (intervals=as.numeric(names(tabla)))
    (f=as.numeric(c))
    muestra.hist=rep(0,n)
    fcumul=cumsum(f)
    muestra.hist[1: f[1] ]=runif(f[1], min=breaks[1], max=breaks[2])
    for(i in 2:length(f)){
      a=f[i-1];b=f[i+1]
      muestra.hist[(fcumul[i-1]+1):fcumul[i]]=runif(f[i], min =breaks[i], max = breaks[i+1])
     }  
  }else{
    set.seed(seed)
    muestra.intervalos=sample(1:length(rf), replace=T, prob=rf,size = n) 
    tabla=table(muestra.intervalos)
    c=unlist(tabla)
    (intervals=as.numeric(names(tabla)))
    (f=as.numeric(c))
    muestra.hist=rep(0,n)
    fcumul=cumsum(f)
    muestra.hist[1: f[1] ]=runif(f[1], min=breaks[1], max=breaks[2])
    for(i in 2:length(f)){
    set.seed(seed)
    muestra.hist[(fcumul[i-1]+1):fcumul[i]]=runif(f[i], min =breaks[i], max = breaks[i+1])
      
    }
    
  }
  return(muestra.hist)
}

#### EXAMPLE 1: Normal distribution ######

set.seed(12345)
y=rnorm(10000)
y=rgamma(10000,shape = 1,scale = 1)
info=hist(y, breaks="Sturges", col="darkgray")
probability=info$counts/sum(info$counts)

simulated=hist.sample(info$breaks,info$counts/sum(info$counts),100000)

par(mfrow=c(1,2))

hist(y, breaks="Sturges", col="darkgray");hist(simulated,breaks=info$breaks)

#### EXAMPLE 2: Gamma distribution ######
set.seed(12345)
y=rgamma(1000,shape = 1,scale = 1)
info=hist(y, breaks="Sturges", col="darkgray")
probability=info$counts/sum(info$counts)

simulated=hist.sample(info$breaks,info$counts/sum(info$counts),100000)

par(mfrow=c(1,2))

hist(y, breaks="Sturges", col="darkgray");hist(simulated,breaks=info$breaks)
