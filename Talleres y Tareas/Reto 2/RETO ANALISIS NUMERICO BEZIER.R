library(bezier)
library(grid)
library(gridBezier)
library(plot3D)
library(plotly)

x <- c(0,
       0.374,
       0.760,
       0.778,
       0.988,
       1)
y <- c(0.8,
       0.8,
       0.541,
       0.146,
       0.082,
       0)

grid.newpage()

#-----HALLAR LOS PUNTOS DADOS POR BEZIEL
#-----CURVA 1---------------------------
bezier1<- BezierGrob(c(x[1:4]),c(y[1:4]),gp=gpar(lwd=3),stepFn = nSteps(40))#EJECUTA LA FUNCION 
vBezier1<- BezierPoints(bezier1)#OBTIENE LOS VALORES DEL ALGORITMO
#-----CURVA 2-------------
bezier2<- BezierGrob(c(x[4],x[4:6]),c(y[4],y[4:6]),gp=gpar(lwd=3),stepFn = nSteps(20))
vBezier2<- BezierPoints(bezier2)

vBez1x=as.vector(vBezier1$x)#QUITA LAS UNIDADES
vBez1y=as.vector(vBezier1$y)#QUITA LAS UNIDADES
plot(vBez1x,vBez1y)

vBez2x=as.vector(vBezier2$x)#QUITA LAS UNIDADES
vBez2y=as.vector(vBezier2$y)#QUITA LAS UNIDADES

datosX<-c(vBez1x,c(vBez2x[2:length(vBez2x)]))#SE UNEN LAS DOS CURVAS
datosY<-c(vBez1y,c(vBez2y[2:length(vBez2y)]))
print(length(datosX))
print(length(datosY))
#----------------------Boquilla----------------------
auxX=vBez2x#AUXILIAR PARA LA CURVA 2
for(i in 1:6){
  auxX=auxX*0.6+2
  datosX<-c(datosX,vBez1x,auxX[2:length(auxX)])#AÑADE LA NUEVA CURVA DOS Y LOS DATOS EN REVERSA PARA QUE NO SE SOBREPONGAN
  datosY<-c(datosY,vBez1y,vBez2y[2:length(vBez2y)])#**TAL VEZ CUANDO SEA 3D NO SEA NECESARIO EL REV
}
#GRAFICA CUADRANTE 1
plot(datosX,datosY,ylim = c(-4,4),xlim = c(-7,7), type = 's')

#----------------------SEMICIRCULO-------------------
bezier3<- BezierGrob(c(0,0,0.8,0.8),c(0.8,0.8,0.8,0),gp=gpar(lwd=3),stepFn = nSteps(40))#EJECUTA LA FUNCION 
vBezier3<- BezierPoints(bezier3)#OBTIENE LOS VALORES DEL ALGORITMO
vBez3x=as.vector(vBezier3$x)#QUITA LAS UNIDADES
vBez3y=as.vector(vBezier3$y)#QUITA LAS UNIDADES
parte3Y=vBez3y
parte3X=vBez3x*-1
lines(vBez3x,vBez3y)
print(length(datosX))
print(length(datosY))
for(i in 1:3){
  datosX<-c(datosX,vBez3x)
  datosY<-c(datosY,vBez3y)
}
auxZ=1
datosZ<-c(rep(1,59))
for(i in 1:6){
  auxZ=auxZ-0.001
  datosZ<-c(datosZ,rep(auxZ,59))
}
for(i in 1:3){
  auxZ=auxZ-0.001
  datosZ<-c(datosZ,rep(auxZ,40))
}
print(length(datosX))
print(length(datosY))
print(length(datosZ))
plot_ly(x=datosX,y=datosY,z=datosZ)
#--------------------TAPA---------------
tapaX<-vector()
tapaY<-vector()
tapaZ<-vector()
vBez3z<-vector()
acum=0
for(i in 1:10){
  acum=acum+1
  vBez3x=vBez3x*(1-(acum*0.01))
  tapaX<-c(tapaX,vBez3x)
  vBez3y=vBez3y*(1-(acum*0.01))
  tapaY<-c(tapaY,vBez3y)
}
for(i in 1:10){
  auxZ=auxZ-0.001
  tapaZ<-c(tapaZ,rep(auxZ,40))
}
for(i in 1:10){
  acum=acum+1
  vBez3x=vBez3x*(1-(acum*0.01))
  tapaX<-c(tapaX,vBez3x)
  vBez3y=vBez3y*(1-(acum*0.01))
  tapaY<-c(tapaY,vBez3y)
}
for(i in 1:10){
  tapaZ<-c(tapaZ,rep(auxZ,40))
}
datosX<-c(datosX,tapaX)
datosY<-c(datosY,tapaY)
datosZ<-c(datosZ,tapaZ)
plot_ly(x=datosX,y=datosY,z=datosZ)

#DUPLICA CUADRANTE 1 Y GENERA CUADRANTE 2
datos2CX=datosX
datos2CY=datosY*-1
datos2CZ=datosZ

datosX=c(datosX,datos2CX)
datosY=c(datosY,datos2CY)
datosZ=c(datosZ,datos2CZ)

plot_ly(x=datosX,y=datosY,z=datosZ)

#------------------CUADRANTE 3--------------------
parte3Z<-vector()
datosX<-c(datosX,rep(parte3X,10))
datosY<-c(datosY,rep(parte3Y,10))
auxZ=1.001
for(i in 1:10){
  auxZ=auxZ-0.001
  parte3Z<-c(parte3Z,rep(auxZ,40))
}
datosZ<-c(datosZ,parte3Z)
datosX<-c(datosX,tapaX*-1)
datosY<-c(datosY,tapaY)
datosZ<-c(datosZ,tapaZ)

plot_ly(x=datosX,y=datosY,z=datosZ)

#-----------------CUADRANTE 4-----------------------
parte4Z=parte3Z
parte4X=parte3X
parte4Y=parte3Y*-1
datosX<-c(datosX,rep(parte4X,10))
datosY<-c(datosY,rep(parte4Y,10))
datosZ<-c(datosZ,parte4Z)
datosX<-c(datosX,tapaX*-1)
datosY<-c(datosY,tapaY*-1)
datosZ<-c(datosZ,tapaZ)
plot_ly(x=datosX,y=datosY,z=datosZ,type = 's')
