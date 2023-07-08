# Defino las variables
a<-0.05
k<-5000
n<-1000
lambda<-1.5

# Pivote T1
T1<-function(sample, mean, lambda){
  return( sqrt(length(sample)) * (mean - (1/lambda)) / mean )
}

# Pivote T2
T2<-function(sample, lambda){
  return( 2 * lambda * sum(sample) )
}

# Vectores para almacenar las k replicaciones
pivot_1_samples<-rep(0,k)
pivot_2_samples<-rep(0,k)

# Inicializo los vectores para guardar los intervalos de confianza
# (Se debe poder hacer de otra forma)
interval_1<-rep(0,2)
interval_2<-rep(0,2)

# Por cada replicación...
for (i in 1:k) {
  # Genero una muestra de valores con distribuciones exponenciales con el parámetro dado
  sample<-rexp(n,lambda)
  
  # Calculo el promedio de la muestra
  mean<-mean(sample)
  
  # Aplico la función pivote 1 a cada muestra
  pivot_1_samples[i]<-c(T1(sample, mean, lambda))
  # Aplico la función pivote 2 a cada muestra
  pivot_2_samples[i]<-c(T2(sample, lambda))
  
  # Calculo el intervalo de confianza para el pivote 1
  interval_1[1]<-(1 / (-(qnorm(1-(0.05/2))*mean / sqrt(n)) + mean))
  interval_1[2]<-(1 / ((qnorm(1-(0.05/2))*mean / sqrt(n)) + mean))
  
  # Calculo el intervalo de confianza para el pivote 2
  # Debería guardarse para cada k
  interval_2[1]<-(qchisq((0.05/2), 2*n) / (2 * sum(sample)))
  interval_2[2]<-(qchisq(1-(0.05/2), 2*n) / (2 * sum(sample)))
}

