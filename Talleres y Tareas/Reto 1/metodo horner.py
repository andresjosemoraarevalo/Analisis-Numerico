a0 = int(input ("\n Ingresa el termino a la 0 >> "))
a1 = int(input ("\n Ingresa el termino a la 1 >> "))
a2 = int(input ("\n Ingresa el termino a la 2 >> "))
a3 = int(input ("\n Ingresa el termino a la 3 >> "))
a4 = int(input ("\n Ingresa el termino a la 4 >> "))

a=[a4,a3,a2,a1,a0]
def horner(a,x):#implementa metodo de horner
        if len(a)==1: #En la lista estan los coeficientes del polinomio
            return a[0] #Y en X el valor a evaluar
        else:
            return a[0] + x * horner(a[1:],x)
x = 1
print(horner(a,x))
        
