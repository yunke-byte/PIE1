moure_esquerra <- function(i){
  i - ((1 + i) / 4)
}

moure_dreta <- function(i){
  i + ((1 - i) / 4)
}

R1 <- function(x){  
  runif(1) <= ((1 - x)^2 / ((1 + x)^2 + (1 - x)^2)) 
}

R2 <- function(x){  
  runif(1) <= (1 - x) / 2
}

R3 <- function(x){  
  runif(1) <= (1 + x) / 2
}

crea_histograma <- function(escenari, regla, N, T) {
  if (escenari == "S1") {
    x <- runif(N, min = -1, max = 1)
  } else if (escenari == "S2") {
    x <- rnorm(N, mean = 0, sd = sqrt(1/13))
  }
  
  for (t in 1:T){
    for (i in 1:N){
      if (regla(x[i])) { 
        x[i] <- moure_esquerra(x[i])
      } else { 
        x[i] <- moure_dreta(x[i])
      }
    }
  }
  return(x)
}

# Assignació d'usuaris i passos
N <- 10000
T <- 15

# Regles (l'utilitzarem com a funció i com a str)
regles <- list(R1 = R1, R2 = R2, R3 = R3)


par(mfrow = c(1, 3))

i <- 1
for (regla in regles) {
  
  # Creació d'histogrames S1 i S2 amb les tres regles
  resultat_S1 <- crea_histograma("S1", regla, N, T)
  resultat_S2 <- crea_histograma("S2", regla, N, T)
  
  esp_S1 <- mean(resultat_S1)
  esp_S2 <- mean(resultat_S2)
  
  var_S1 <- var(resultat_S1)
  var_S2 <- var(resultat_S2)
  
  cat("Regla:", names(regles)[i], "\n")
  cat("   Esperanza en S1:", esp_S1, "Variança en S1: ", var_S1, "\n")
  cat("   Esperanza en S2:", esp_S2, "Variança en S2", var_S2, "\n")
  # Histograma S1
  hist(resultat_S1, 
       main = paste("Comparació", names(regles)[i]),
       
       xlab = "Opinions",
       ylab = "Usuaris",
       col = rgb(1, 0, 0, 0.5),  # Vermell
       border = "black",
       freq = F
       )  
  
  # Histograma distribució S2
  hist(resultat_S2, 
       
       col = rgb(0, 0, 1, 0.5),  
       border = "black",
       freq = F,
       add = TRUE)  
  i <- i + 1
  
  
  
}
