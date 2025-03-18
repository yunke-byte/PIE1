# Funcions de moviment
moure_esquerra <- function(i){
  i - ((1+i)/4)
}

moure_dreta <- function(i){
  i + ((1-i)/4)
}

#Regles (Retorna TRUE si mostra contingut d'esquerres)
R1 <- function(x){  
  runif(1) <= ((1-x)^2 / ((1+x)^2 + (1-x)^2)) 
}

R2 <- function(x){  
  runif(1) <= (1-x)/2
}

R3 <- function(x){
  runif(1) <= (1+x)/2
}

n<- 8
R4 <- function(x){  
  runif(1) <= ((1+x)^n) / ((1-x)^n + (1+x)^n) 
}

#Funcions de sortir

surt_oposat <- function(x){  # L'usuari abandona al veure contingut del bàndol oposat(p = 0.25)
  runif(1) <= 0.25  
}

surt_cansat <- function(x){  # L'usuari abandona al veure contingut del mateix bàndol(p = 0.05)
  runif(1) <= 0.05
}

####################################################
############ Funcions per l'apartat e) #############
####################################################

esquerra <- function(x){#Retorn True si l'usuari és d'esquerres
  return (x < 0)
}

#Funció que retorna la distribució triada
assigna_distribucio <- function(escenari){
  if (escenari == "S1") {#Distribució uniforme
    return(x <- runif(N, min = -1, max = 1))
  } else if (escenari == "S2") {#Distribució normal
    return(x <- rnorm(N, mean = 0, sd = sqrt(1/13)))
  }
}

############################!!!!Important!!!!#################################
######Funció per simular els continguts consumits al llarg del temps##########

continguts_consumits <- function(distribucio, regla, N, T) {
  # Escollim distribucio
  x <- assigna_distribucio(distribucio)
  
  # Creem dos vectors de mida N, un per comptar els continguts consumits
  # L'altre vector ens servirà com un booleà per comprovar si l'usuari ja ha sortit(per no comptar-ho)
  comptador <- rep(0, N)
  ha_sortit <- rep(FALSE, N)
  
  # Bucle de T passos i N usuaris, on es va augmentant el comptador pels usuaris actius
  
  for (t in 1:T) {
    for (i in 1:N) {
      if (!ha_sortit[i]) {#No ha sortit
        # Suma un contingut més a l'usuari i
        comptador[i] <- comptador[i] + 1
        
        if (regla(x[i])) {
          # Es mostra contingut d'esquerres
          
          # Comprovem en cas d'usuari de dretes
          if (!esquerra(x[i])) {
            # El contingut és oposat
            if (surt_oposat(x[i])) { 
              #Si surt, assignem el valor True a la posició i del vector ha_sortit
              ha_sortit[i] <- TRUE
              next
            } else {
              #Si no surt, l'usuari es mou cap a esquerres
              x[i] <- moure_esquerra(x[i])
            }
          } else {
            # Comprovem en cas d'usuari d'esquerres
            if (surt_cansat(x[i])) {
              #Probabilitat de sortir (p=0.05)
              ha_sortit[i] <- TRUE
              next
            } else {
              #L'usuari d'esquerres es torna més d'esquerres
              x[i] <- moure_esquerra(x[i])
            }
          }
        } 
        #Es mostra contingut de dretes i es segueix la mateixa lògica però a l'inrevès
        else {
          
          if (esquerra(x[i])) {
            
            if (surt_oposat(x[i])) {
              ha_sortit[i] <- TRUE
              next
            } else {
              x[i] <- moure_dreta(x[i])
            }
          } else {
            
            if (surt_cansat(x[i])) {
              ha_sortit[i] <- TRUE
              next
            } else {
              x[i] <- moure_dreta(x[i])
            }
          }
        }
      }
    }
  }
  # Es torna el valor de només aquells usuaris que han sortit
  return(comptador[ha_sortit])
}

####################################################
################# Constants#########################
####################################################

# Paràmetres fixes
N <- 10000
T <- 20


regles <- list(R1 = R1, R2 = R2, R3 = R3, R4 = R4)


par(mfrow = c(2, 4))

#Histogrames de la distribució a XT

i<- 1
for (regla in regles) {
  
  # Creació d'histogrames S1 i S2 amb les tres regles
  resultat_S1 <- crea_histograma("S1", regla, N, T)
  resultat_S2 <- crea_histograma("S2", regla, N, T)
  
  # Histograma S1
  hist(resultat_S1, 
       main = paste("Comparació", names(regla)[i]),
       
       xlab = "Opinions",
       ylab = "Usuaris",
       xlim = c(-1,1),
       col = rgb(1, 0, 0, 0.5),  # Vermell
       breaks = 10,
       border = "black",
       
  )  
  
  # Histograma distribució S2
  hist(resultat_S2, 
       
       col = rgb(0, 0, 1, 0.5),  
       border = "black",
       breaks =10,
       xlim = c(-1,1),
       add = TRUE)  
  i <- i + 1
}


#Gràfics superposats de contingut consumit



i<- 1
for (regla in regles) {
  
  # Creació d'histogrames S1 i S2 amb les tres regles
  resultat_S1 <- continguts_consumits("S1", regla, N, T)
  resultat_S2 <- continguts_consumits("S2", regla, N, T)
  
  # Histograma S1
  hist(resultat_S1, 
       main = paste("Comparació", names(regles)[i]),
       
       xlab = "Nombre de Continguts",
       ylab = "Usuaris abandonant",
       col = rgb(1, 0, 0, 0.5),  # Vermell
       border = "black",
       freq = F,
       ylim = c(0,0.5)
  )  
  
  # Histograma distribució S2
  hist(resultat_S2, 
       
       col = rgb(0, 0, 1, 0.5),  #Blau
       border = "black",
       freq = F,
       ylim = c(0,0.5),
       add = TRUE)  
  i < i + 1
}




