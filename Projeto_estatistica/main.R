library(formattable)
library(tidyverse)
library(plyr)

INICIAR = function(){
  
  df <- data.frame(
    
    #NOME
    NOME = df0$NOME, 
    
    #IDADE
    IDADE = df0$IDADE,
    
    #Formação
    ESCOLARIDADE = df0$ESCOLARIDADE,
    
    #PESO
    PESO = df0$PESO,
    
    #ALTURA
    ALTURA = df0$ALTURA,
    
    stringsAsFactors = FALSE)
  
  setTable()
  
  calcularIMC(df)
  
  formattable(df, list(
    IMC = formatter("span", 
                    style = ~ style(color = ifelse(IMC >= 18.5 & IMC < 24.99, "green", "red"))),
    ESCOLARIDADE = formatter("span", style = x ~ ifelse(x == "A", 
                                                        style(color = "green", font.weight = "bold"), NA)),
    area(col = c(PESO)) ~ normalize_bar("pink", 0.2),
    area(col = c(ALTURA)) ~ normalize_bar("yellow", 0.5)
  ))
  
}

setTable = function(){
  
  df0 <- read.table("input.csv", header=TRUE, sep=",")
  
}


calcularIMC = function(df){
  df["IMC"] <- NULL
  for(i in 1:nrow(df)) {
    df$IMC[i] <- as.numeric(formatC((df$PESO[i] / (df$ALTURA[i])^2),  digits=3))
    
  }
}


df1 <- data.frame(
  
  MEDIA = formatC(mean(df$IMC), digits = 3),
  
  MEDIANA = formatC(median(df$IMC), digits = 3),
  
  DECIL4 = formatC(quantile(df$IMC, prob = seq(0, 1, length = 11), type = 5)[5], digits = 3),
  
  DESVIO_PADRAO = formatC(sd(df$IMC), digits = 3))

formattable(df1)


formattable(ImcFrequency, list(
  area(col = c(Freq)) ~ normalize_bar("pink", 0.6)
))

gerarBoxplot = function(){
  print("Gráfico em Boxplot")
  print(boxplot(df$IMC))
}

gerarHistograma = function(){
  hist(df1, main = "Histograma dos dados", xlab = "Dados", ylab="Frequência")
}

receberFrequencias = function(){
  ImcFrequency <- data.frame( FREQUENCIA <-table(df$IMC))
}
INICIAR()
