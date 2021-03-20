#inicialização bibliotecas
library(readxl)

#leitura de arquivo xlsx
dim_tempo = read_xlsx("data/dim_time/DIM_TEMPO.xlsx", sheet = 'DIM_TEMPO')


#create time dimensaion
write.csv(x = dim_tempo, file = "data/tempo.csv", fileEncoding = "UTF-8", row.names = FALSE, quote = TRUE)

rm(dim_tempo)
