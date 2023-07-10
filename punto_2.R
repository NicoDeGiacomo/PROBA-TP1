# Establezco el seed para poder repetir los resultados
set.seed(1)

# Defino las variables
a<-0.05
k<-5000
sizes<-c(10, 30, 100, 1000)
lambdas<-c(1, 5)

# Defino un data frame para guardar los resultados
results_t1 <- data.frame()
results_t2 <- data.frame()

# Inicializo los vectores para guardar los intervalos de confianza
ic_t1<-c()
ic_t2<-c()

# Por cada lambda...
for (lambda in lambdas) {
  
  # Por cada n...
  for (n in sizes) {
    
    # Variables para guardar resultados parciales
    long_ic_t1 <- c()
    long_ic_t2 <- c()
    cobertura_t1 <- c()
    cobertura_t2 <- c()
    
    # Por cada replicación...
    for (m in 1:k) {
    
      # Genero una muestra de valores con distribuciones exponenciales con el parámetro dado
      sample<-rexp(n,lambda)
      
      # Calculo el promedio de la muestra
      mean<-mean(sample)
      
      # Calculo el intervalo de confianza para el pivote 1
      ic_t1[1]<-(1 / ((qnorm(1-(0.05/2))*mean / sqrt(n)) + mean))
      ic_t1[2]<-(1 / (-(qnorm(1-(0.05/2))*mean / sqrt(n)) + mean))
      
      # Calculo el intervalo de confianza para el pivote 2
      ic_t2[1]<-(qchisq((0.05/2), 2*n) / (2 * sum(sample)))
      ic_t2[2]<-(qchisq(1-(0.05/2), 2*n) / (2 * sum(sample)))
      
      # Para esta iteración, guardo la longitud del IC y la cobertura.
      long_ic_t1 <- append(long_ic_t1, ic_t1[2] - ic_t1[1])
      cobertura_t1 <- append(cobertura_t1, ic_t1[1] < lambda && lambda < ic_t1[2])
      long_ic_t2 <- append(long_ic_t2, ic_t2[2] - ic_t2[1])
      cobertura_t2 <- append(cobertura_t2, ic_t2[1] < lambda && lambda < ic_t2[2])
      
    }
    
    # Guardo los resultados en el data frame
    results_t1 <- rbind(results_t1, list(lambda=lambda, n=n, ic=mean(long_ic_t1), coverage=mean(cobertura_t1)))
    results_t2 <- rbind(results_t2, list(lambda=lambda, n=n, ic=mean(long_ic_t2), coverage=mean(cobertura_t2)))
  }
}

results_t1
results_t2