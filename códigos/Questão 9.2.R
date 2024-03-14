# Questão 9.2
n <- 10
x <- c(2.2,3.5,3.4,6.7,6.2,8.2,9.2,7.9,9.0,10.1)
theta.hat <- mean(x)

# Estimadores das reamostragens
theta.hat.i <- NULL

for (i in 1:n) {
  theta.hat.i[i] <- mean(x[-i])
}

# Resultado conhecido (para média amostral)
mean(x)==mean(theta.hat.i)

# Calcular os pseudo-valores de jackknife
theta.til <- NULL

for (i in 1:n) {
  theta.til[i] <- n*theta.hat-(n-1)*theta.hat.i[i]
}


# Estimador pontual de Jackknife
theta.jack <- mean(theta.til)


# Dispersão estimada
var.theta <- (1/(n*(n-1)))*sum((theta.til-theta.jack)^2)


# Usando o pacote bootstrap
library(bootstrap)
jackknife(x,theta=function(x)mean(x))

# Estimador pontual de jackknife (com pacote)
mean(jackknife(x,function(x)mean(x))$jack.values)

# Fazer estimação intervalar (intervalo de 1-alpha% de confiança)
