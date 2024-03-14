# ------------------------------------------------------------------------------
# QUESTÃO 1.3
# ------------------------------------------------------------------------------

# ITEM A
# ------------------------------------------------------------------------------
# Gerar valores de uma uniforme(a,b)
# Etapas
# 1) Gerar valores de uniforme(0,1)
# 2) Obter expressão para x
# 3) Gerar n valores de x

# Parâmetros
a <- 0
b <- 10
n <- 1000
u <- runif(n,min=0,max=1)
x <- u*(b-a)+a
summary(x)

# Visualização com base R
hist(x, freq = F, 
     xlab = "Valores", 
     ylab = "Densidade", 
     main = "Histograma do vetor X")
boxplot(x, ylab = "Valores observados", main = "Boxplot do vetor X")
densidade <- density(x)
plot(densidade, xlim = c(0,10), xlab = "Valores", ylab = "Densidade", main = "Gráfico de Densidade")

# Visualização com ggplot (melhorar a visualização com o ggplot2)
library(ggplot2)
ggplot(data.frame(x = x), aes(x)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histograma de x", x = "Valores de x", y = "Frequência")

boxplot_data <- data.frame(x = x)
boxplot <- ggplot(boxplot_data, aes(y = x)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de x", y = "Valores de x")

# Criar uma curva de densidade com ggplot2
density_data <- data.frame(x = x)
density <- ggplot(density_data, aes(x)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Curva de Densidade de x", x = "Valores de x", y = "Densidade")

# Exibir os gráficos
print(boxplot)
print(density)


# ITEM B
# ------------------------------------------------------------------------------
# Gerar valores de uma variável aleatória X cuja função densidade é f=n*x^(n-1)
# 1) Gerar valores de uniforme(0,1)
# 2) Obter expressão para x
# 3) Gerar n valores de x

# Parâmetros
n <- 1000
u <- runif(n,min=0,max=1)
x <- u^(1/n)
summary(x)

# Visualização com base R
hist(x, freq = F, 
     xlab = 'Valores observados', 
     ylab = 'Densidade', 
     main = 'Histograma do vetor X')

boxplot(x, ylab = 'Valores observados', main = 'Boxplot do vetor X', freq = F)

densidade <- density(x)
plot(densidade, xlab = "Valores", ylab = "Densidade", main = "Gráfico de Densidade")


# ITEM C
# ------------------------------------------------------------------------------
# Gerar valores de uma variável aleatória X~Exponencial(lambda), E(X)=1/lambda
# 1) Gerar valores de uniforme(0,1)
# 2) Obter expressão para x
# 3) Gerar n valores de x

# Parâmetros
lambda <- 10
n <- 1000
u <- runif(n,min=0,max=1)
x <- -(log(1-u))/lambda
summary(x)

# Visualização com o base R
# Aqui é possível comparar o comportamento usando rexp(n,lambda)
hist(x, freq = F, main = 'Histograma com Método da Transformação Inversa',
     xlab = 'Valores observados',
     ylab = 'Densidade')
exp <- rexp(n,rate=lambda)
hist(exp, freq = F, main = 'Histograma com rexp',
     xlab = 'Valores observados',
     ylab = 'Densidade')
boxplot(x, main = 'Método da Transformação inversa', freq = F)
boxplot(exp, main = 'Usando rexp', freq = F)


# ITEM D
# ------------------------------------------------------------------------------
# Gerar valores de uma variável aleatória X~Gama(n,lambda), E(X)=n/lambda

lambda <- 1
n <- 100000
x <- NULL

for (i in 1:n) {
  u=runif(n,min=0,max=1)    # o vetor u é composto por n números de uma uniforme(0,1)
  y=-(log(1-u))/lambda      # o vetor y é composto por n números da transformação
  x[i]=sum(y)               # o i-ésimo elemento do vetor x, sendo x valor gerado de uma gama(n,lambda)
}
summary(x)

# Visualização no base R
hist(x, freq = F, main = 'Valores de uma Gama(n,lambda)',
     xlab = 'Valor de x', ylab = 'Densidade')
boxplot(x)
