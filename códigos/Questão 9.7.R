library(bootstrap)

# Inserindo dados
patch <- patch
n <- nrow(patch)

# Estatística de interesse
theta.hat <- mean(patch$y)/mean(patch$z)

y <- patch$y
z <- patch$z


# Estimadores de cada reamostra
theta.hat.i <- NULL

for (i in 1:n) {
  theta.hat.i[i] <- mean(y[-i])/mean(z[-i])
}

# Pseudo-valores de Jackknife
theta.til <- n*theta.hat-(n-1)*theta.hat.i

# Estimador de Jackknife
theta.jack <- mean(theta.til)


# Viés do Estimador de Jackknife
vies.jack <- (n-1)*((1/n)*sum(theta.hat.i)-theta.hat)


# Variância estimada do estimador de jackknife


# Erro padrão = estimativa do desvio-padrão do estimador 



# Estimativa jackknife intervalar (95% de confiança) (Caso n fosse grande)
alpha <- 0.05
li = theta.jack-qt(1-alpha/2,n-1)*
ls = theta.jack+qt(1-alpha/2,n-1)*

(intervalo <- c(li,ls))

