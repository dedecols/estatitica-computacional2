# Jackknife: theta.hat.i, theta.til

# Inserindo dados do problema
n <- 5
x<-c(2.1,1.7,4,1.8,5)


########## PARTE 1 ##########
theta.hat <- min(x)


theta.hat.i <- NULL
for (i in 1:n) {
  theta.hat.i[i] <- min(x[-i]) ## atenção aqui, theta.hat.i tem que ser igual ao theta.hat (media, var etc)
}


theta.til <- NULL
for (i in 1:n) {
  theta.til[i] <- n*theta.hat-(n-1)*theta.hat.i[i]
}


# Estimador pontual de Jackknife
theta.jack <- mean(theta.til)

# Estimador intervalar





# ----- Usando o pacote
library(bootstrap)
jackknife(x,function(x)var(x))

# Estimador pontual
theta.jack.lib <- mean(jackknife(x,function(x)var(x))$jack.values)

# Estimador intervalar
alpha <- 0.05
li <- theta.jack.lib-qt(1-alpha/2,n-1)*jackknife(x,function(x)var(x))$jack.se
ls <- theta.jack.lib+qt(1-alpha/2,n-1)*jackknife(x,function(x)var(x))$jack.se

intervalo <- c(li,ls)


mean(jackknife(x,function(x)min(x))$jack.values)
mean(n*min(x)-(n-1)*jackknife(x,function(x)min(x))$jack.values)


