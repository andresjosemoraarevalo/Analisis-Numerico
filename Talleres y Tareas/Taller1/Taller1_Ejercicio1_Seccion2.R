entero<-function(num) {
  sum=0
  i=1
  if(num<=1){
    print("no se puede convertir")
  }
  while (num > 0) {
    digit=num %% 2
    num=floor(num / 2)
    sum=sum + digit * i
    i=i * 10
  }
  return(sum)
}

print(entero(2))