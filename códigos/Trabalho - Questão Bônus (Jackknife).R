library(readr)
library(bootstrap)


amostra <- read_table("BancoSalarios.txt")
View(amostra)
n <- nrow(amostra)


# Dividir os dados para homens e mulheres
# Homens: x
homens <- amostra[amostra$Sexo=="M",]
x <- homens$Salario
# Mulheres: y
mulheres <- amostra[amostra$Sexo=="F",]
y <- mulheres$Salario



# ---- HOMENS ---- #
# Estimador com todos os dados da amostra
theta.hat1 <- mean(x)
# Estimadores das reamostras
theta.hat1.i <- NULL
for (i in 1:n) {
  theta.hat1.i[i]=mean(x[-i])
}
# Pseudo-valor de Jackknife
theta.til1 <- n*theta.hat1-(n-1)*theta.hat1.i
# Estimador de Jackknife
theta.jack1 <- mean(theta.til1)
# Vício do estimadors
vicio.jack1 <- (n-1)*((1/n)*sum(theta.hat1.i-theta.hat1))
# Erro padrão
erropadrao1 <- jackknife(x,function(x)mean(x))$jack.se

theta.jack1
vicio.jack1
erropadrao1




# ---- MULHERES ---- #
# Estimador com todos os dados da amostra
theta.hat2 <- mean(y)
# Estimadores das reamostras
theta.hat2.i <- NULL
for (i in 1:n) {
  theta.hat2.i[i]=mean(y[-i])
}
# Pseudo-valores de Jackknife
theta.til2 <- n*theta.hat2-(n-1)*theta.hat2.i
# Estimador de Jackknife
theta.jack2 <- mean(theta.til2)
# Vício do estimador
vicio.jack2 <- (n-1)*((1/n)*sum(theta.hat2.i-theta.hat2))
# Erro padrão
erropadrao2 <- jackknife(y,function(y)mean(y))$jack.se

theta.jack2
vicio.jack2
erropadrao2



# Como as amostras não são grandes o suficiente então não é possível supor distribuição assintótica dos estimadores. Ou seja, não temos como construir um intervalo de confiança. A alternativa é observar a variabilidade dos dados para afirmar algo sobre a diferença da média salarial entre homens e mulheres.
# Após o cálculo dos erros padrão podemos usá-los como um desvio-padrão para termos uma faixa de valores com maior probabilidade de concentrar o parâmetor. Fazendo isso, ainda vemos uma distância considerável entre a média salarial de homens e mulheres. 

# Faixa da média salarial dos homens
min1 <- theta.jack1-erropadrao1
max1 <- theta.jack1+erropadrao1
(faixa.homens <- c(min1,max1))

# Faixa da média salarial das mulheres
min2 <- theta.jack2-erropadrao2
max2 <- theta.jack2+erropadrao2
(faixa.mulheres <- c(min2,max2))




