polinomio1 <- c(2,0,-3,3,-4)
metodo1<-function(polinomio,x){#No había que implementarlo pero se implementó
  size=length(polinomio)
  size=size-1 
  cont=0
  acum=0
  for(i in 0:size){
    cont=cont+i
    acum=acum+polinomio[size-(i-1)]*x^i
  }
  print(paste("El numero de multiplicaciones es:",cont))
  print(paste("El resultado es: ",acum))
}
metodo2<-function(polinomio,x){#Metodo de horner
  size=length(polinomio)
  size=size-1
  cont=0
  acum=0
  for(i in 0:size){
    if(i<=1){
      acum=acum+polinomio[size-(i-1)]*(x^i)
      cont=cont+i
    }
    else{
      acum=acum+polinomio[size-(i-1)]*x*x^(i-1)
      cont=cont+2
    }
  }
  print(paste("El numero de multiplicaciones es:",cont))
  print(paste("El resultado es: ",acum))
  return (acum)
}
metodo1(polinomio1,1)
metodo2(polinomio1,1)
polinomio_3<-rep(1,51)#lleno de 1
resultado_o=metodo2(polinomio_3,1.0001)
resultado_e=(((1.0001)^51)-1)/(1.0001-1)
print(paste("El resultado equivalente es: ",resultado_e))
error=resultado_o-resultado_e
print(paste("El error es de: ",error))
