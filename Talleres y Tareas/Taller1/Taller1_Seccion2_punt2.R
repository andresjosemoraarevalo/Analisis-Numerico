
#creamos los primeros números naturales al cuadrado
n<- y^2
cuadrado <- function(n , val, x_0,err){
  deltax_k  =  1
  es  =  0
  er ? 
  while (abs ( deltax_k ) >  er ) {
    deltax_k  = (( val / ( x_k ** ( n - 1 ))) -  x_k ) / n
    x_k  =  x_k  +  deltax_k
    it  =  it  +  1
  }
}

cat ( " El resultado es " , cuadrado ( n , val , x_0 , err ))


#graficando h(x) vs x
plot.function(f,0,2, pch =19, main = "h(x) en función de x",xlab = " X ",ylab = " h(x) ",col ="black")

#imprime la relación de error -> Convergencia lineal
points(cuadrado, cuadrado, col = "blue")
lines(cuadrado, cuadrado, col = "blue")
