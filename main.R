rm(list = ls())
library("stringr")
library("tuple")
library("rlang")
library("ggplot2")
library("dplyr")
library("tidyr")

options(max.print=.Machine$integer.max)

for(f in list.files("funcoes")){
  load(paste0("funcoes/",f))
}

rm(f)
k <- read.csv(file = "metro_rec_tratado.csv")
Metrô <- ler("METROREC.csv")
#' escrevendo tabela completa tratada
write.csv(Metrô, file = "metro_rec_tratado.csv")

erros <- conjura(Metrô)
#' escrevendo erros dos cdvs
write.csv(erros, file = "erros_cdv.csv",na = "")


resultado <- segrega.cdv(erros)
dados <- calcula.cdv(resultado,Metrô)
calcula_reta_simples(dados$`1N01T`[1])
Metrô %>% filter(is.na(tempo_abertura) & is.na(tempo_encerramento)) %>% select("tempo_abertura","tempo_encerramento")

Metrô %>%filter(!(is.na(tempo_abertura) &is.na(tempo_encerramento)) &
                  (is.na(tempo_abertura)|is.na(tempo_encerramento) ))%>%select("tempo_abertura","tempo_encerramento")

Metrô %>% filter(solicitacao=="000002") %>% mutate(solicitacao=2,solicitacao=ifelse(solicitacao==2,50,20)) %>% select(solicitacao)


with open('output.csv','w') as out:
for row in data:
for col in row:
out.write('{0};'.format(col))
out.write('\n')
