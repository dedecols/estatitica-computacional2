# Exemplos 6.1

# Item a

# Observar o comportamento da função (investigar raiz)
plot(function(x)f(x))
plot(function(x)f.derivada(x), xlim = c(-5,5))

# Funções
f <- function(x){
  funcao=exp(x)-2
  
  return(funcao)
}

f.derivada <- function(x){
  funcao=exp(x)
  
  return(funcao)
}


# Caso 1: x0=0.7
x0 <- 0.7

x1 <- x0-(f(x0)/f.derivada(x0))
x2 <- x1-(f(x1)/f.derivada(x1))
x3 <- x2-(f(x2)/f.derivada(x2))


# Caso 2: x0=0
x0 <- 0

x1 <- x0-(f(x0)/f.derivada(x0))
x2 <- x1-(f(x1)/f.derivada(x1))
x3 <- x2-(f(x2)/f.derivada(x2))


# Para automatizar é preciso usar um critério de parada
precisao <- 0.000001

condicao <- abs(x[i+1]-x[i])>precisao


# EXEMPLO 6.2

# Item a
# Ponto inicial para o vetor (alfa,beta)
alpha=2
beta=2

# Informações do problema
n=10
soma.x <- 5.9035
soma.ln <- -6.4813

# Comportamento da função gama com os parâmetros inciais
plot(function(x)dgamma(x,alpha,beta), xlim=c(0,5), xlab="Valores de x", ylab="Gamma(2,2)")

# Verossimilhança

plot(function(x)dgamma(x,alpha,beta)*dgamma(x,alpha,beta)*dgamma(x,alpha,beta)*dgamma(x,alpha,beta)*
       dgamma(x,alpha,beta)*dgamma(x,alpha,beta)*dgamma(x,alpha,beta)*dgamma(x,alpha,beta)
     *dgamma(x,alpha,beta)*dgamma(x,alpha,beta),xlim=c(0,1.2))

x <- rgamma(10,alpha,beta)

verossimilhanca <- function(x,alpha,beta){
  n=10
  funcao=n*alpha*log(beta)-n*log(gamma(alpha))+(alpha-1)*(sum(log(x))-beta*(sum(log(x))))
  
  return(funcao)
}

verossimilhanca(x,2,2)


plot(function(x)verossimilhanca(x,alpha,beta))

