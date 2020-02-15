ECUACION<-function(y,t)
{
  
  y<-(6+(2.13*t^ 2)-(0.0013*t^4))
  round(y , 9)
  round (t, 9)
  #redondeamos la cifra en una relativamente grande
      return(y)
  
  x = seq(9, 10, by=0.2)
  y = x^2 + 0.01
  plot(y,t, pch = 19, cex = 0.5, col = "blue", asp= 1)
  abline(h = 0, v =0 , lty = 3)
}

#donde y es la altura en metros
#t es el tiempo en segundos
