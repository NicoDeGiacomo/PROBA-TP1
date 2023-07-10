############ Establezco el seed para poder repetir los resultados ############
set.seed(1)

############ Funciones utiles ############

# Devuelve el extremo inferior y superior para el pivote T1
T1_IC <- function(sample, n) {
  mean <- mean(sample)
  sup <- (1 / (-(qnorm(1-(0.05/2))*mean / sqrt(n)) + mean))
  inf <- (1 / ((qnorm(1-(0.05/2))*mean / sqrt(n)) + mean))
  return(c(inf, sup))
}
# Devuelve el extremo inferior y superior para el pivote T2
T2_IC <- function(sample, n) {
  sum <- sum(sample)
  sup <- (qchisq(1 - (0.05 / 2), 2 * n) / (2 * sum))
  inf <- (qchisq((0.05 / 2), 2 * n) / (2 * sum))
  return(c(inf, sup))
}

############ Comienzo del Script ############

# Defino las variables
a <- 0.05
k <- 5000
sizes <- c(10, 30, 100, 1000)
lambdas <- c(1, 5)

# Defino data frames para guardar los resultados
t1_results <- data.frame()
t2_results <- data.frame()

# Por cada lambda...
for (lambda in lambdas) {
  
  # Por cada n...
  for (n in sizes) {
    
    # Variables para guardar resultados parciales
    t1_long_ic <- c()
    t1_cobertura <- c()
    
    t2_long_ic <- c()
    t2_cobertura <- c()
    
    # Por cada replicación...
    for (m in 1:k) {
      
      # Genero una muestra de valores con distribuciones exponenciales con el parámetro dado
      sample <- rexp(n, lambda)
      
      # Calculo el intervalo de confianza
      t1_ic_inf <- T1_IC(sample, n)[1]
      t1_ic_sup <- T1_IC(sample, n)[2]
      
      t2_ic_inf <- T2_IC(sample, n)[1]
      t2_ic_sup <- T2_IC(sample, n)[2]
      
      # Para esta iteración, guardo la longitud del IC y la cobertura.
      t1_long_ic <- append(t1_long_ic, t1_ic_sup - t1_ic_inf)
      t1_cobertura <- append(t1_cobertura, t1_ic_inf < lambda && lambda < t1_ic_sup)
      
      t2_long_ic <- append(t2_long_ic, t2_ic_sup - t2_ic_inf)
      t2_cobertura <- append(t2_cobertura, t2_ic_inf < lambda && lambda < t2_ic_sup)
    }
    
    # Guardo los resultados en el data frame
    t1_results <- rbind(t1_results, list(lambda=lambda, n=n, ic=mean(t1_long_ic), coverage=mean(t1_cobertura)))
    t2_results <- rbind(t2_results, list(lambda=lambda, n=n, ic=mean(t2_long_ic), coverage=mean(t2_cobertura)))
  }
}

t1_results
t2_results
