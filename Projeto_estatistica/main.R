library(formattable)
library(tidyverse)
library(plyr)

INICIAR = function(){
  
  df <- data.frame(
  
    ID = 1:30,
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
    ID = color_tile("white", "orange"),
    IMC = formatter("span", 
                    style = ~ style(color = ifelse(IMC >= 18.5 & IMC < 24.99, "green", "red"))),
    ESCOLARIDADE = formatter("span", style = x ~ ifelse(x == "A", 
                                                        style(color = "green", font.weight = "bold"), NA)),
    area(col = c(PESO)) ~ normalize_bar("pink", 0.2),
    area(col = c(ALTURA)) ~ normalize_bar("yellow", 0.5)
  ))
  
}

setTable = function(){
  
  df0 <- read.table("DATA.txt", header=TRUE, sep=",")
  
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
  
  DESVIO_PADRAO = formatC(sd(df$IMC), digits = 3),
  
  MODA = getmode(df$IMC)
  
  )

formattable(df1)

df2 <- data.frame(
  
  MEDIA = formatC(mean(df$PESO), digits = 3),
  
  MEDIANA = formatC(median(df$PESO), digits = 3),
  
  DECIL4 = formatC(quantile(df$PESO, prob = seq(0, 1, length = 11), type = 5)[5], digits = 3),
  
  DESVIO_PADRAO = formatC(sd(df$PESO), digits = 3),
  
  MODA = getmode(df$PESO)
  
)



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

formattable(ImcFrequency, list(
  
  area(col = c(Frequencia)) ~ normalize_bar("grey", 0.2)
  
 
))


gerarBoxplot = function(){
  print("Gráfico em Boxplot")
  print(boxplot(df$IMC))
}


# Pie Chart with Percentages
slices <- c(1,1,1,7,2,2,16)
lbls <- c("Infantil Incompleto", "Doutorado Completo", "Fundamental Incompleto", "Médio Completo", "Médio Incompleto", "Superior Completo", "Superior Incompleto")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Gráfico de Setores")


gerarHistograma = function(){
 a = hist(df$IMC, main = "Histograma dos dados", xlab = "Dados", ylab="Frequência")
}
oi <- data.frame(
  valores = df$ALTURA
)
h1 <- ggplot(oi, aes(valores)) + geom_histogram() +
  xlab("Altura(m)") + ylab("Frequencia")


receberFrequencias = function(){
  ImcFrequency <- data.frame( FREQUENCIA <-table(df$ESCOLARIDADE))
}
INICIAR()
