#install.packages("readxl")
library("readxl")
options(scipen=100)
enem_data <- read_excel("ENEM_AL_EXCEL_AJUS_OK.xlsx")

cidade <- subset(enem_data, NO_MUNICIPIO_RESIDENCIA =='Maceió')

#barplot - Sexo e redação
notas_redacao.cut <- cut(cidade$NU_NOTA_REDACAO, breaks = quantile(cidade$NU_NOTA_REDACAO))
sexo_notas <- table(cidade$TP_SEXO,notas_redacao.cut)
barplot(sexo_notas, main = "Nota Redação dividida por sexo", 
        col=c("purple", "blue"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)),cex.axis = 0.8)

#nota geral - idade
notas.cut <- cut(cidade$NOTA_ENEN, breaks = quantile(cidade$NOTA_ENEN))
idade.cut <- cut(cidade$NU_IDADE, breaks = quantile(cidade$NU_IDADE))
nota_idade <- table(notas.cut,idade.cut)
barplot(nota_idade, main = "Nota por idade", 
        col=c("green", "blue","red","purple"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)),ylim = c(0, 4000))

#grafico de pizza: tipo de escola
escolas <- table(cidade$TP_ESCOLA)
labs <- paste(c("Não declarado = ", "Privado = ", "Pública = "),
                round(escolas/length(cidade$TP_ESCOLA) * 100, 
                      digits=2), "%")
pie(escolas,labels = labs,col = c("gray","yellow","green"),
    main="Relação de tipos de escola no ENEM",
    cex=1.1)

#lingua - tipo de escola
lingua_escola <- table(cidade$TP_LINGUA,cidade$TP_ESCOLA)
barplot(lingua_escola, main = "Relação Lingua X Escola", 
        col=c("gray", "yellow"), beside=TRUE, legend=TRUE, 
        args.legend = list(x = "topright", inset = c(0, -0.4)),ylim = c(0, 12000))
