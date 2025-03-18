moure_esquerra <- function(i){
  i - ((1+i)/4)
}

moure_dreta <- function(i){
  i + ((1-i)/4)
}

R1<- function(x){#retorna TRUE si ensenyem contingut d'esquerres
  runif(1) <= ((1-x)**2 / ((1+x)**2 + (1-x)**2)) 
}

R2<- function(x){#retorna TRUE si ensenyem contingut d'esquerres
  runif(1) <= (1-x)/2
}

R3<- function(x){#retorna TRUE si ensenyem contingut d'esquerres
  runif(1) <= (1+x)/2
}

x<- 0.5 #assignem el valor desitjat
a<- x #ens assegurem que la x no varia

if (R1(x)){
  (a<-moure_esquerra(x))
} else{
  (a<-moure_dreta(x))
}

if (R2(x)){
  (a<-moure_esquerra(x))
} else{
  (a<-moure_dreta(x))
}

if (R3(x)){
  (a<-moure_esquerra(x))
} else{
  (a<-moure_dreta(x))
}