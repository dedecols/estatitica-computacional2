# Questão 1.5

# a - X tem distribuição de Bernoulli(p)
# se usar a função if então precisa usar for
# com ifelse
u = runif(1000, min=0, max=1)
p = 0.5

x <- ifelse(u<p, 0, 1)
table(x)


# b - X tem distribuição criada pela professora
u = runif(1000, min=0, max=1)
x <- ifelse(u<=0.2,1,
     ifelse(u<=0.35,2,
     ifelse(u<=0.6,3,4)))
table(x)


# c - X tem distribuição Uniforme discreta que assume valores de 1 a 10
u = runif(1000, min=0, max=1)
x <- ifelse(u<=0.1,1,
     ifelse(u<=0.2,2,
     ifelse(u<=0.3,3,
     ifelse(u<=0.4,4,
     ifelse(u<=0.5,5,
     ifelse(u<=0.6,6,
     ifelse(u<=0.7,7,
     ifelse(u<=0.8,8,
     ifelse(u<=0.9,9,10)))))))))
table(x)


# d - Y tem distribuição Binomial(n,p)
u = runif(1000, min=0, max=1)
p = 0.5
x <- ifelse(u<1-p,0,1) 
y <- sum(x)
table(x)

n=1000

for (i in 1:n) {
  x <- ifelse(runif(n, min=0, max=1)<1-p,0,1)
  y[i] <- sum(x)
}

x11()
par(mfrow=c(2,1))
hist(y,freq=F)


hist(rbinom(600, n, p),freq = F)
