## Punto 1.a Reto 1

n<- k #Grado del polinomio es un grado que es asignado ya que puede variar
x0<-(y)
a0<-(z) # Término independiente del polinomio de la ecuacion a0x^n + a1x^n-1 + ...
a<-c(1, 2, 3, k + 1) # coeficientes del polinomio desde a1 hasta ... k que es an
y<-a[n]
z<-a[n-1]
j<-n-1
while (0<j){
  y<-x0*y+a[j]
  z<-x0*z+y
  j<-j-1}
print(z)
y<-x0*y+a0
y

