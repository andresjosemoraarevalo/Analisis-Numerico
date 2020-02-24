library(pracma)
f <- function(x) sin(x)
p <- taylor(f, 0 , 5)#Al hallar los valores del polinomio, la funcion retorna un valor Double
                     #Por esta razón ya tiene aproximacion doble
x <- seq(-2.8125,2.8125, length=50)#50 valores son necesarios
yf <- f(x)
yp <- polyval(p, x)#Evalua el polinomio para todos los puntos
plot(x, yf, main= "Aproximación de Taylor", ylab="sin(x)", type = "l", col = "gray", lwd = 3)
lines(x, yp, col = "red")
legend("topleft",c("sin(x)","Taylor aprox"),fill=c("gray","red"))

#Error absoluto
ef <- f(2.8125)
ep <- polyval(p,2.8125)
errorR=((ep-ef)/ep)*100
print(paste("Error Relativo: ",errorR,"%"))