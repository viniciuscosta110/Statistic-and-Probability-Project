install.packages("readxl")
library("readxl")
enem_data <- read_excel("ENEM_AL_EXCEL_AJUS_OK.xlsx")
notas_enem <- enem_data["NOTA_ENEN"]

freq <- table(notas_enem)
frequel <- prop.table(freq)
freqAc <- cumsum(freq)
frequelAc <- cumsum(frequel)

tabelaf <- cbind(freq, freqAc, frequel = round(frequel*100, digits=4), frequelAc = round(frequelAc*100, digits=4))
tabelaf

boxplot(notas_enem, main = "Notas do Enem", col="blue", border="black")
