# Método de Otimização 
# Exercício 1 
somatorio.x <- 5.9035
somatorio.logx <- -6.4813
alpha <- 2
beta <- 2
n=10

derivadas <- function(alpha,beta,somatorio.x,somatorio.logx){
  f1=n*log(beta)-n*digamma(alpha)+somatorio.logx
  f2=((n*alpha)/beta)-somatorio.x
  
  return(c(f1,f2))
}

derivadas(2,2,somatorio.x,somatorio.logx)

# Derivadas da verossimilhança
# Em relação a alpha
f1=n*log(beta)-n*digamma(alpha)+somatorio.logx
# Em relação a beta
f2=((n*alpha)/beta)-somatorio.x

# Derivadas das derivadas
derivada.f1.alpha <- -n*trigamma(alpha)
derivada.f1.beta <- n/beta
derivada.f2.alpha <- n/beta
derivada.f2.beta <- (-n*alpha)/beta^2

# Matriz Jacobiana
J <- matrix(c(derivada.f1.alpha, derivada.f1.beta,
         derivada.f2.alpha, derivada.f2.beta),
       byrow = T, ncol = 2, nrow = 2)

# Existe matriz inversa
det(J)

#Encontrar Inversa
library(matlib)
inv(J)
solve(J)
inv(J)==solve(J)

