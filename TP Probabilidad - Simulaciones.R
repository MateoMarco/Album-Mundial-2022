#PARTE 1

#a)

paquete <- sample(640,5,replace=TRUE)

#b)

genPaquete <- function (figusTotal,figusPaquete){
  paquete <- sample(figusTotal,figusPaquete,replace=TRUE)
  return (paquete)
}

#c)

cuantosPaquetes <- function (figusTotal,figusPaquete){
  album <- c()
  nPaquetes <- 0
  figus <- 0
  while (length(album)<figusTotal){
    figus <- genPaquete(figusTotal,figusPaquete)
    album <- c(figus,album)
    album <- unique(album)
    album <- sort(album)
    nPaquetes <- nPaquetes+1
  }
  return(nPaquetes)
}

cuantosPaquetes(640,5)

#PARTE 2

#a)

#Función de probabilidad de N
#N: "Cantidad de paquetes necesarios para completar el álbum."

tablaprob <- function(nRep){
  nSimulados <- c()
  while (length(nSimulados)<nRep){
    nx <- cuantosPaquetes(640,5)
    nSimulados <- c(nx, nSimulados)
  }
  maxPaquete <- max(nSimulados)
  fN <- rep(0,maxPaquete)
  for (i in 1:length(nSimulados)){
    w <- nSimulados[i]
    fN[w] <- fN[w]+1
  }
  prob <- fN/nRep
  return(prob)
}

probN <- function (N, nRep){
  tabla <- tablaprob (nRep)
  if (N>length(tabla)){
    prob<-0
  }
  else {
    prob <- tabla[N]
  }
  return (prob)
} 

#Gráfico de función de probabilidad de N

graficoN <- function(nRep){
  yN <- tablaprob(nRep)
  x_paquetes <- c(1:length(yN))
  barplot(yN, names.arg = x_paquetes, main = "Funcion de probabilidad",xlab= "Paquetes (N)", ylab="Probabilidad estimada", space=0.01, xlim=c(0,length(yN)))
  abline(v=901.05,col="forest green")
}
graficoN(100)


#Función de probabilidad de C
#C: "Cantidad de plata que debe gastar para completar el álbum."

tablaGastos <- function(nRep){
  nSimulados <- c()
  while (length(nSimulados)<nRep){
    nx <- cuantosPaquetes(640,5)
    nSimulados <- c(nx, nSimulados)
  }
  plata <- 20*nSimulados
  maxPlata <- max(plata)
  fN <- rep(0,maxPlata)
  for (i in 1:length(plata)){
    w <- plata[i]
    fN[w] <- fN[w]+1
  }
  prob <- fN/nRep
  return(prob)
}

probC <- function (c, nRep){
  tabla <- tablaGastos (nRep)
  if (c>length(tabla)){
    prob<-0
  }
  else {
    prob <- tabla[c]
  }
  return (prob)
} 

# Gráfico de función de probabilidad de C

graficoC <- function (nRep){
  yC <- tablaGastos(nRep)
  x_plata <- c(1:length(yC))
  barplot(yC, names.arg = x_plata, main = "Función de probabilidad de C", xlab= "Costo ($)", 
          ylab="Probabilidad estimada",xlim=c(0,length(yC)*1.25), col=heat.colors(15))
  abline(v=18021, col="forest green")
} 
graficoC(100)


#Función de densidad de T
#T: "Tiempo necesario hasta completar el álbum"

tablaTiempo <- function (nRep){
  nSimulados <- c()
  tiempo <- c()
  while (length(nSimulados)<nRep){
    nx <- cuantosPaquetes(640,5)
    nSimulados <- c(nx, nSimulados)
  }
  for (i in 1:length(nSimulados)){
    tiempo_i <- rexp(nSimulados[i]-1,1)
    tiempo[i] <- sum(tiempo_i)
  }
  return(tiempo)
}

densidadT <- function(nRep){
  tabla <- tablaTiempo(nRep)
  hist(tabla,main= "Histograma vs Densidad", xlab="Cantidad de días para completar el álbum",ylab= "Densidad estimada",freq=FALSE,col=hcl.colors(15,palette = "viridis"))
  lines(density(tabla), col="skyblue",lwd=3)
  abline (v=901.05, col="forest green")
}

densidadT(100)

#b)

#Esperanza de N
esperanzaN <- function (nRep){
  nSimulados <- c()
  while (length(nSimulados)<nRep){
    nx <- cuantosPaquetes(640,5)
    nSimulados <- c(nx, nSimulados)
  }
  eN <- mean(nSimulados)
  return(eN)
}
esperanzaN(100)

#Esperanza de C

esperanzaC <- function(nRep){
  
  nSimulados <- c()
  while (length(nSimulados)<nRep){
    nx <- cuantosPaquetes(640,5)
    nSimulados <- c(nx, nSimulados)
  }
  plata <- nSimulados*20
  
  eC <- mean(plata)
  return(eC)
  
}
esperanzaC(100)

#Esperanza de T

esperanzaT <- function (nRep){
  tabla <- tablaTiempo(nRep)
  eT <- mean(tabla)
  return(eT)
}


#c)

#Nrep={10, 200, 500, 1000, 10000}

#10

tablaprob(10)
graficoN(10)
esperanzaN(10)

tablaGastos(10)
graficoC(10)
esperanzaC(10)

tablaTiempo(10)
densidadT(10)
esperanzaT(10)

#200

tablaprob(200)
graficoN(200)
esperanzaN(200)

tablaGastos(200)
graficoC(200)
esperanzaC(200)

tablaTiempo(200)
densidadT(200)
esperanzaT(200)

#500

tablaprob(500)
graficoN(500)
esperanzaN(500)

tablaGastos(500)
graficoC(500)
esperanzaC(500)

tablaTiempo(500)
densidadT(500)
esperanzaT(500)

#1000

tablaprob(1000)
graficoN(1000)
esperanzaN(1000)

tablaGastos(1000)
graficoC(1000)
esperanzaC(1000)

tablaTiempo(1000)
densidadT(1000)
esperanzaT(1000)

#10000

tablaprob(10000)
graficoN(10000)
esperanzaN(10000)

tablaGastos(10000)
graficoC(10000)
esperanzaC(10000)

tablaTiempo(10000)
densidadT(10000)
esperanzaT(10000)

