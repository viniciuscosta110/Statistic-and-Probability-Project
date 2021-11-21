install.packages("readxl")
library("readxl")
options(scipen=100)
enem_data <- read_excel("ENEM_AL_EXCEL_AJUS_OK.xlsx")
notas_enem <- enem_data["NOTA_ENEN"]

freq <- table(notas_enem)
frequel <- prop.table(freq)
freqAc <- cumsum(freq)
frequelAc <- cumsum(frequel)

#Tabelas com Notas_ENEM
tabelaf <- cbind(freq, freqAc, frequel = round(frequel*100, digits=4), frequelAc = round(frequelAc*100, digits=4))

#Boxplot Notas_ENEM
boxplot(notas_enem, main = "Notas do Enem", col="blue", border="black")

#Histograma Notas_ENEM
hist(freq, main = "Frequência de notas", 
     xlab = "Frequência", ylab = "Quantidade",
     col="blue", border="black",
     xlim = c(0, 30), ylim = c(0, 8000), cex.axis=0.8)

#Barplot com quartis (nota e sexo)
notas.cut <- cut(enem_data$NOTA_ENEN, breaks = quantile(enem_data$NOTA_ENEN))
sexo_notas <- table(enem_data$TP_SEXO, notas.cut)
barplot(sexo_notas, main = "Nota dividido por sexo", 
        col=c("purple", "blue"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)),ylim = c(0, 12000))

#barplot - idade e nota
notas.cut <- cut(enem_data$NOTA_ENEN, breaks = quantile(enem_data$NOTA_ENEN))
idade.cut <- cut(enem_data$NU_IDADE, breaks = quantile(enem_data$NU_IDADE))
nota_idade <- table(notas.cut,idade.cut)
barplot(nota_idade, main = "Nota por idade", 
        col=c("green", "blue","red","purple"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)),ylim = c(0, 10000))

#Barplot - Relação sexo e treineiro
traineiro_idade <- table(enem_data$IN_TREINEIRO,enem_data$TP_SEXO)
barplot(traineiro_idade, main = "Relação Treineiro X Sexo", 
        col=c("purple", "blue"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)))

#barplot - Sexo e redação
##resolver o 1000
notas_redacao.cut <- cut(enem_data$NU_NOTA_REDACAO, breaks = quantile(enem_data$NU_NOTA_REDACAO))
sexo_notas <- table(enem_data$TP_SEXO,notas_redacao.cut)
barplot(sexo_notas, main = "Nota Redação dividida por sexo", 
        col=c("purple", "blue"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)),ylim = c(0, 12000),cex.axis = 0.8)

#algoplor - idade - nota de matematica
notas_matematica <- cut(enem_data$NU_NOTA_MT, breaks = quantile(enem_data$NU_NOTA_MT))
idade.cut <- cut(enem_data$NU_IDADE, breaks = quantile(enem_data$NU_IDADE))
matematica_idade <- table(idade.cut,notas_matematica)
barplot(matematica_idade, main = "Nota de matematica por idade", 
        col=c("green", "blue","red","purple"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)),ylim = c(0, 10000))
