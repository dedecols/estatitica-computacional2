# EXEMPLO (SLIDE 9)

# DADOS DO EXEMPLOS
# f(x) - variável aleatória desejada
f <- function(x){
        valor_f = 20*x*(1-x)^3
        return(valor_f)
}

# g(x) - função de densidade para envelopar
g <- function(x){
        valor_g = 1
        return(valor_g)
}

# CONHECENDO AS DENSIDADES ANTES DE ENCONTRAR "C"
# Comportamento da densidade f(x)
plot(function(x)f(x), ylab = "f(x)", xlim = c(0,1), ylim = c(0,2))

# Comportamento da densidade g(x)
plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 2), ylab = "g(x)")
abline(h = g(0), col = "blue")





optimize(f, interval = c(0,1), maximum = TRUE)
1/4

# FINALIZAR O EXEMPLO DEPOIS




# ===============================================================================
# QUESTÃO 2.1 - gerar valores de uma normal padrão (ponto de máximo é quando x=0)
# ===============================================================================
# item a: a partir de uma uniforme(-10,10)
# Densidade de X~Normal(0,1)
f <- function(numero){
  valor_f=dnorm(numero,mean=0,sd=1)
  return(valor_f)
}
# f(0) é quase 0.4

# Densidade de G - tem que ser outra coisa que não função (idealmente)
g <- 1/20


# Encontramos c fazendo a derivada de f/g e igualando o resultado a zero. Assim temos o ponto crítico que pode ser máximo ou mínimo, porém, como combinado em aula assumiremos que é ponto de máximo sem fazer o teste da segunda derivada. 
c=20/sqrt(2*pi) #aproximadamente 8


# Vetor que receberá os valores aceitos para distribuição X a partir de uma uniforme(-10,10)
x=NULL

i <- 1

while (length(x)<1000) {
  xprop <- runif(1,min=-10,max=10)
  u <- runif(1,min=0,max=1)
  pa <- f(xprop)/(c*g)
  
  ifelse(u<pa, ((x[i]=xprop)  & (i <- i+1)), i <- i)
}
x
hist(x)
# ===============================================================================
# item b - a partir de uma Cauchy Padrão cuja densidade é g.
g <- function(x){
  valor_cauchy=1/(pi*(1+x^2))
  return(valor_cauchy)
}

# Valor de c
c <- dnorm(0)/(1/(pi*(1-0^2)))

# X proposto (a partir da Cauchy Padrão)
rcauchy(1)
plot(function(x)dcauchy(x))
