library(readxl)
dados <- read_excel("BancoDengue.xls", sheet = "Final")
View(dados)

# Amostras
amostra1 <- dados$"2002"
amostra2 <- dados$"2003"
amostra3 <- dados$"2004"
amostra4 <- dados$"2005"
amostra5 <- dados$"2006"
amostra6 <- dados$"2007"
amostra7 <- dados$"2008"

# Numa análise inicial vemos grande variabilidade de dados principalmente nos anos de 2002 e 2008. Os anos de 2003, 2004 e 2005 apresentam baixa variabilidade. Observar a variabilidade pode ser interessante porque ela pode apontar para grande números de casos em diferentes regiões.   
boxplot(amostra1,amostra2,amostra3,amostra4,amostra5,amostra6,amostra7)





#########################################
########## MÉTODO DE BOOTSTRAP ##########
#########################################



# Número de réplicas/reamostras: B
B <- 1000
# Vetor com o estimador bootstrap para cada ano
theta.bootstrap.vetor <- NULL




# ---- REAMOSTRAS DO ANO DE 2002 ---- #
theta.reamostra.vetor <- NULL

for (i in 1:B) {
  posicao <- sample(1:length(amostra1),length(amostra1),replace=TRUE)
  reamostra <- amostra1[posicao]
  theta.reamostra.vetor[i] <- mean(reamostra)
}

# Estimador de bootstrap para o ano de 2002
mean(theta.reamostra.vetor) # bootstrap2002
hist(theta.reamostra.vetor, freq=F, main="Variabilidade do estimador para o ano de 2002")
theta.bootstrap.vetor[1] <- mean(theta.reamostra.vetor)

# Medidas de variabilidade: viés, erro-padrão e intervalo de confiança
# Viés
vies1 <- mean(theta.bootstrap.vetor)-mean(dados$"2002")
# Variância
var1 <- var(theta.reamostra.vetor)
# Erro-padrão
ep1 <- sqrt(var1)
# Intervalo de confiança de 95% para o parâmetro
quantile(theta.reamostra.vetor,c(0.025,0.975))










# ---- Reamostras do ano de 2003 ---- #
theta.reamostra.vetor <- NULL

for (i in 1:B) {
  posicao <- sample(1:length(amostra2),length(amostra2),replace=TRUE)
  reamostra <- amostra2[posicao]
  theta.reamostra.vetor[i] <- mean(reamostra)
}

# Estimador de bootstrap para o ano de 2003
mean(theta.reamostra.vetor)
hist(theta.reamostra.vetor, freq=F, main="Variabilidade do estimador para o ano de 2003")
theta.bootstrap.vetor[2] <- mean(theta.reamostra.vetor)

# Medidas de variabilidade: viés, erro-padrão e intervalo de confiança
# Viés
vies2 <- mean(theta.reamostra.vetor)-mean(dados$"2003")
# Variância
var2 <- var(theta.reamostra.vetor)
# Erro-padrão
ep2 <- sqrt(var2)
# Intervalo de confiança de 95% para o parâmetro
quantile(theta.reamostra.vetor,c(0.025,0.975))










# ---- Reamostras do ano de 2004 ---- #
theta.reamostra.vetor <- NULL

for (i in 1:B) {
  posicao <- sample(1:length(amostra3),length(amostra3),replace=TRUE)
  reamostra <- amostra3[posicao]
  theta.reamostra.vetor[i] <- mean(reamostra)
}

# Estimador de bootstrap para o ano de 2004
mean(theta.reamostra.vetor)
hist(theta.reamostra.vetor, freq=F, main="Variabilidade do estimador para o ano de 2004")
theta.bootstrap.vetor[3] <- mean(theta.reamostra.vetor)

# Medidas de variabilidade: viés, erro-padrão e intervalo de confiança
# Viés
vies3 <- mean(theta.reamostra.vetor)-mean(dados$"2004")
# Variância
var3 <- var(theta.reamostra.vetor)
# Erro-padrão
ep3 <- sqrt(var3)
# Intervalo de confiança de 95% para o parâmetro
quantile(theta.reamostra.vetor,c(0.025,0.975))










# ---- Reamostras do ano de 2005 ---- #
theta.reamostra.vetor <- NULL

for (i in 1:B) {
  posicao <- sample(1:length(amostra4),length(amostra4),replace=TRUE)
  reamostra <- amostra4[posicao]
  theta.reamostra.vetor[i] <- mean(reamostra)
}

# Estimador de bootstrap para o ano de 2005
mean(theta.reamostra.vetor)
hist(theta.reamostra.vetor, freq=F, main="Variabilidade do estimador para o ano de 2005")
theta.bootstrap.vetor[4] <- mean(theta.reamostra.vetor)

# Medidas de variabilidade: viés, erro-padrão e intervalo de confiança
# Viés
vies4 <- mean(theta.reamostra.vetor)-mean(dados$"2005")
# Variância
var4 <- var(theta.reamostra.vetor)
# Erro-padrão
ep4 <- sqrt(var4)
# Intervalo de confiança de 95% para o parâmetro
quantile(theta.reamostra.vetor,c(0.025,0.975))










# ---- Reamostras do ano de 2006 ---- #
theta.reamostra.vetor <- NULL

for (i in 1:B) {
  posicao <- sample(1:length(amostra5),length(amostra5),replace=TRUE)
  reamostra <- amostra5[posicao]
  theta.reamostra.vetor[i] <- mean(reamostra)
}

# Estimador de bootstrap para o ano de 2006
mean(theta.reamostra.vetor)
hist(theta.reamostra.vetor, freq=F, main="Variabilidade do estimador para o ano de 2006")
theta.bootstrap.vetor[5] <- mean(theta.reamostra.vetor)

# Medidas de variabilidade: viés, erro-padrão e intervalo de confiança
# Viés
vies5 <- mean(theta.reamostra.vetor)-mean(dados$"2006")
# Variância
var5 <- var(theta.reamostra.vetor)
# Erro-padrão
ep5 <- sqrt(var5)
# Intervalo de confiança de 95% para o parâmetro
quantile(theta.reamostra.vetor,c(0.025,0.975))










# ---- Reamostras do ano de 2007 ---- #
theta.reamostra.vetor <- NULL

for (i in 1:B) {
  posicao <- sample(1:length(amostra6),length(amostra6),replace=TRUE)
  reamostra <- amostra6[posicao]
  theta.reamostra.vetor[i] <- mean(reamostra)
}

# Estimador de bootstrap para o ano de 2007
mean(theta.reamostra.vetor)
hist(theta.reamostra.vetor, freq=F, main="Variabilidade do estimador para o ano de 2007")
theta.bootstrap.vetor[6] <- mean(theta.reamostra.vetor)

# Medidas de variabilidade: viés, erro-padrão e intervalo de confiança
# Viés
vies6 <- mean(theta.reamostra.vetor)-mean(dados$"2007")
# Variância
var6 <- var(theta.reamostra.vetor)
# Erro-padrão
ep6 <- sqrt(var6)
# Intervalo de confiança de 95% para o parâmetro
quantile(theta.reamostra.vetor,c(0.025,0.975))










# ---- Reamostras do ano de 2008 ---- #
theta.reamostra.vetor <- NULL

for (i in 1:B) {
  posicao <- sample(1:length(amostra7),length(amostra7),replace=TRUE)
  reamostra <- amostra7[posicao]
  theta.reamostra.vetor[i] <- mean(reamostra)
}

# Estimador de bootstrap para o ano de 2008
mean(theta.reamostra.vetor)
hist(theta.reamostra.vetor, freq=F, main="Variabilidade do estimador para o ano de 2008")
theta.bootstrap.vetor[7] <- mean(theta.reamostra.vetor)

# Medidas de variabilidade: viés, erro-padrão e intervalo de confiança
# Viés
vies7 <- mean(theta.reamostra.vetor)-mean(dados$"2008")
# Variância
var7 <- var(theta.reamostra.vetor)
# Erro-padrão
ep7 <- sqrt(var7)
# Intervalo de confiança de 95% para o parâmetro
quantile(theta.reamostra.vetor,c(0.025,0.975))





# Estimadores de bootstrap para cada ano
theta.bootstrap.vetor

# Conclusões
# A partir dos dados analisados verificamos que os anos de 2002, 2007 e 2008 foram epidêmicos para casos de dengue no município do Rio de Janeiro. 
# Após 2002 o município do Rio de Janeiro apresentou anos não epidêmicos. Entre 2003 e 2005 as taxas médias foram bem baixas. No ano de 2006 é possível notar considerável aumento em relação aos anos anteriores, mas ainda não tanto para ser classificado como ano epidêmico. Nos últimos dois anos, 2007 e 2008, a taxa média já estava bem maior a ponto de se afirmar que foram anos epidêmicos para o município.  




