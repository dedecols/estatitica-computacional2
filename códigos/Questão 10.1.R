LSAT<-c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
GPA<-c(3.39,3.30,2.81,3.03,3.44,3.07,3,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)


# Estimador pontual para a amostra original
cor(LSAT,GPA)

B <- 100
resultados <- numeric(B)

for (i in 1:B) {
  # Posição dos indivíduos selecionados para reamostra
  posicao <- sample(1:length((LSAT)), length(LSAT), replace = TRUE)
  # i-ésima reamostra
  reamostra.l <- LSAT[posicao]
  reamostra.g <- GPA[posicao]
  # Calcular a correlação da reamostra selecionada
  resultados[i] <- cor(reamostra.l,reamostra.g)
}

summary(resultados)


bootstrap.cor <- function(LSAT, GPA, B, nivel.confianca=0.95) {
  # Inicializar um vetor para armazenar os resultados
  resultados <- numeric(B)
  # Realizar o Bootstrap
  for (i in 1:B) {
    # Posição dos indivíduos selecionados para reamostra
    posicao <- sample(1:length((LSAT)), length(LSAT), replace = TRUE)
    # i-ésima reamostra
    reamostra <- c(LSAT[posicao],GPA[posicao])
    x.bootstrap <- x[sample(1:length(x), length(x), replace = TRUE)]
    # Calcular a média da amostra selecionada
    resultados[i] <- mean(x.bootstrap)
  }
  
  
  # Ordenar os resultados
  resultados <- sort(resultados)
  hist(resultados)
  # Calcular o limite inferior e superior do intervalo de confiança
  limite.inferior <- resultados[floor(((1-nivel.confianca)/2)*B)]
  limite.superior <- resultados[ceiling(((1+nivel.confianca)/2)*B)]
  vies <- mean(resultados) - mean(x)
  var.estimador <- var(resultados)
  # Retornar os limites do intervalo de confiança
  return(list(intervalo=c(limite.inferior, limite.superior),
              nivel.confianca= paste0(nivel.confianca*100,'%'),
              vies=vies, 
              var.estimador=var.estimador, 
              EP=sqrt(var.estimador)))
}
