# Exercício 3.1 (mistura de normais)

# Densidade de X
f <-function(x){
  fdp = 0.3*( (1/sqrt(2*pi))*exp(-(x^2)/2) ) + 0.7*( (1/sqrt(4*pi))*exp(-(x-5)^2/4) )
  return(fdp)
}

# Gráfico da Normal Padrão 
curve(dnorm(x), from = -10, to = 15,
      xlab="Valores",
      ylab="Densidade",
      main="Distribuição Normal Padrão")

# Gráfico da Normal(5,4)
curve(dnorm(x,mean=5,sd=2), from=-10, to=15,
      xlab="Valores", ylab="Densidade",
      main="Distribuição Normal(5,4)")

# Gráfico da variável X (usando plot e curve)
plot(function(x)f(x), xlim=c(-10,15),
     xlab="Valores", ylab="Densidade",
     main="Mistura de Normais")

curve(f(x), from=-10, to=15,
      xlab="Valores", ylab="Densidade",
      main="Mistura de Normais")

# Variável auxiliar: Y~Bernoulli(theta) pelo método da Transformação Inversa
U <- runif(1000,min=0,max=1)
theta = 0.7

Y <- ifelse(U>theta,0,1)

table(Y)

barplot(table(Y),
        xlab="Resultado",ylab="Frequência",
        main = "Amostra Aleatória de tamanho 1000 - Bernoulli(0.7)")


# Função alternativa a rnorm para gerar valores para as duas normais
# 1) Método de Aceitação-Rejeição
# 1.1. Normal Padrão usando uma Uniforme(-10,10)
# Passo 1: encontrar c
# Distribuição desejada: Normal Padrão
densidade_np <- function(x){
  densidade_np=(1/sqrt(2*pi))*exp(-(x^2)/2)
  
  return(densidade_np)
}

# Distribuição Proposta: Uniforme(-10,10)
densidade_unif <- function(x){
  densidade_unif=1/20
  
  return(densidade_unif)
}

investigac_a <- function(x){
  razao=densidade_np(x)/densidade_unif(x)
  
  return(razao)
}

otimiza_razao <- optimize(function(x) investigac_a(x),
                          lower=0, upper=10,
                          maximum=TRUE)

max_razao <- as.numeric(otimiza_razao[1])

c <- as.numeric(otimiza_razao[2])

# Verificando resultado da função optmize
c == densidade_np(max_razao)/densidade_unif(max_razao)


# Passo 2: gerar valores de Uniforme(-10,10) e Uniforme(0,1)
z=NULL
i <- 1

while (length(z)<1000) {
  zprop <- runif(1,min=-10,max=10)
  u <- runif(1,min=0,max=1)
  pa <- densidade_np(zprop)/(c*densidade_unif(zprop))
  
  ifelse(u<pa, ((z[i]=zprop)  & (i <- i+1)), i <- i)
}

# Verificando os dados gerados para Normal Padrão
summary(z)
hist(z)
plot(density(z))




# ============================================#
# 1.1.1. Transformando em Normal(5,4)
sigma=2
mi=5

x <- z*sigma+mi

# Valores gerados (Normal(5,4))
head(x)
summary(x)
hist(x)
plot(density(x))





# ============================================#
# 1.2. Normal Padrão usando uma Cauchy padrão
# 1.2.1. Transformando em Normal(5,4)
plot(function(x)dcauchy(x), xlim=c(-10,10))

densidade_np <- function(x){
  densidade_np=(1/sqrt(2*pi))*exp(-(x^2)/2)
  
  return(densidade_np)
}

densidade_cauchy <- function(x){
  densidade=dcauchy(x)
  
  return(densidade)
}

# Passo 1: encontrar c
investigac_b <- function(x){
  razao=densidade_np(x)/densidade_cauchy(x)
  
  return(razao)
}

otimiza_razao <- optimize(function(x) investigac_b(x),
                          lower=0, upper=10,
                          maximum=TRUE)

max_razao <- as.numeric(otimiza_razao[1])

c <- as.numeric(otimiza_razao[2])

# Verificando resultado da função optmize
c == densidade_np(max_razao)/densidade_cauchy(max_razao)


# Passo 2: gerar valores de Cauchy Padrão e Uniforme(0,1)
z_cauchy=NULL
i=1

while (length(z_cauchy)<1000) {
  cauchy_prop <- rcauchy(1)
  u <- runif(1)
  pa <- densidade_np(cauchy_prop)/(c*densidade_cauchy(cauchy_prop))
  
  ifelse(u<pa, ((z_cauchy[i] <- cauchy_prop) & (i <- i+1)), i <- i)
}

# Valores gerados (Normal Padrão)
head(z_cauchy)
summary(z_cauchy)
hist(z_cauchy)
plot(density(z_cauchy))




# ============================================#
# 2) Método de Box Muller para Normal Padrão
# Algoritmo dos slides
# Gerar u1 e u2 da distribuição Uniforme(0,1)
# Faça x1 e x2 como definido abaixo

normal.bm <- function(x){
  n <- x/2
  
  u1 <- runif(n,min=0,max=1)
  u2 <- runif(n,min=0,max=1)
  
  x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
  x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
  
  z <- c(x1,x2)
  
  return(z)
}

z <- normal.bm(1000)
boxplot(z)

# 2.1. Transformando em Normal(5,4)
mi <- 5
sigma <- 2

x <- z*sigma+mi

boxplot(x)











