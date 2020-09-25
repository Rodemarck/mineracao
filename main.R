rm(list = ls())
library("stringr")
library("tuple")
library("rlang")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lubridate")

options(max.print=.Machine$integer.max)

for(f in list.files("funcoes")){
  load(paste0("funcoes/",f))
}

rm(f)
#' lendo e processessando dados
#Metrô <- ler("METROREC.csv")

#' escrevendo tabela completa tratada
#write.csv(Metrô, file = "metro_rec_tratado.csv")

#' lendo dados processados
Metrô <- read.csv(file = "metro_rec_tratado.csv")[2:8]%>%
  mutate(tempo_abertura=strptime(tempo_abertura, "%Y-%m-%d %H:%M:%S"),
         tempo_falha=strptime(tempo_falha,  "%Y-%m-%d %H:%M:%S"),
         tempo_encerramento=strptime(tempo_encerramento,  "%Y-%m-%d %H:%M:%S"),
         t_d=tempo_duracao) %>%
  select(c("solicitacao","localizacao","descricao","tempo_abertura","tempo_falha","tempo_encerramento","t_d")) %>%
  mutate(tempo_duracao=duration(minute=t_d)) %>%
  select(-c("t_d"))
#' procurando erros dos cdvs
#erros <- conjura(Metrô)
#erros <- merge(Metrô,erros)

#' escrevendo erros dos cdvs
#write.csv(erros, file = "erros_cdv.csv")

#' lendo erros dos cdvs
erros <- read.csv(file = "erros_cdv.csv",stringsAsFactors = T)[2:3]

erros %>%
  group_by(solicitacao) %>%
  summary()
erros %>%
  filter(cdv=="2N11T")
str()
calcula_reta_simples(dados$`1N01T`[1])

erros %>% summary()
