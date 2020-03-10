install.packages("PolynomF")#instalar paquete 
help(PolynomF)
library(PolynomF)
x0 <- c(0,1,2)
length(x0)
y0 <- c(10,15,5)
length(y0)
plot(x0,y0, pch=19, xlim = c(0,2),cex=1, col = "red") #ver grafica 
plot(x0,y0, pch=19,cex=1, col = "red", asp=1) #utilizar escala igual
p <- poly_calc(x0, y0)        ## conduce a una falla numérica catastrófica! al utilizar Poly
p                             ## imprime el polinomio
poly
curve(p,add=T)                ## La curva parece indicar que tiene ajuste!!!
range(p(x0) - y0)             ## estos deben estar "cerca de cero"!lo cual no se cumple

p2<-function(x) 10+15*x-7.5*x^2
range(p2(x0)-y0)

p3<-function(x) 10+12.5*x-6.25*x^2
range(p3(x0)-y0)




p1 <- poly_calc(x0 - 84, y0)  ## Propone cambiar el origen soluciona el problema??
p1
range(p1(x0 - 84) - y0)       ## Estos estan cercanos a cero.
plot(p1, xlim = c(80, 89) - 84, xlab = "x0 - 84")
points(x0 - 84, y0, col = "red")
## ¿Podemos ahora escribir el polinomio ?
p0 <- change_origin(p1, -84)  ## intentando cambiar el origen a cero
p0
## conduce a graves problemas numéricos de nuevo!!!!!!!!!!!!
plot(p0, xlim = c(80, 89))
points(x0, y0, col = "red")   ## Mayores errores debido a la  precision (finita)
#### En resumen tenemos valores que oscilan
xi=c(0,.5,1,2,3,4)
xi
yi=c(0,.93,1,1.1,1.15,1.2)
p3 = poly.calc(xi,yi)
p3
range(p3(xi) - yi) 
#polyAjuste
plot(xi,yi, pch = 19, cex=1, col= "red")
curve(p3,add=T,lty=3, lwd=2)
