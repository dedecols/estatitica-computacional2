library(bootstrap)

# Fixando valores
n <- 100
theta <- 10

# Calculando um estimador de jackknife


x <- rpois(n,theta)

# Estimador amostral
theta.hat <- mean(x)

# Estimadores de cada reamostra
theta.hat.i <- jackknife(x,function(x)mean(x))$jack.values

# Pseudo-valores de Jackknife
theta.til <- n*theta.hat-(n-1)*theta.hat.i

# Estimador de Jackknife
theta.jack <- mean(theta.til)



n <- 100
theta <- 10
theta.jack.vetor <- NULL

for (i in 1:n) {
  x <- rpois(n,theta)
  theta.hat <- mean(x)
  theta.hat.i <- jackknife(x,function(x)mean(x))$jack.values
  theta.til <- n*theta.hat-(n-1)*theta.hat.i
  theta.jack.vetor[i] <- mean(theta.til)
}

# Distribuição empírica dos dados
hist(theta.jack.vetor, freq=F)

# Tranformação para t padrão (locação 0 e escala 1)
# thetajack.vetor-theta/desvio
plot(function(x)dt(x,99),-10,10,add=TRUE)

# aumentar o numero de réplicas para ter mais thetajack etc

# a distribuição empírica converge ou não 