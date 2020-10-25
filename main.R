rm(list = ls())
library(dygraphs)
library("stringr")
library("tuple")
library("rlang")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lubridate")
library("pwt8")

for(f in list.files("funcoes")){
    load(paste0("funcoes/",f))
}

rm(f)
#' lendo e processessando dados
#Metrô <- ler2("METROREC.csv")

#' escrevendo tabela completa tratada
#write.csv(Metrô, file = "metro_rec_tratado.csv")

#' lendo dados processados
#Metrô <- read.csv(file = "metro_rec_tratado.csv")[2:9]%>%
#    mutate(tempo_abertura=strptime(tempo_abertura, "%Y-%m-%d %H:%M:%S"),
#           tempo_falha=strptime(tempo_falha,  "%Y-%m-%d %H:%M:%S"),
#           tempo_encerramento=strptime(tempo_encerramento,  "%Y-%m-%d %H:%M:%S"),
#           t_d=tempo_duracao) %>%
#    select(-c("tempo_duracao")) %>%
#    mutate(tempo_duracao=duration(minute=t_d)) %>%
#    select(-c("t_d"))


#' procurando erros dos cdvs
#erros <- conjura(Metrô)
#erros_completos <- Metrô %>% merge(erros, by = "solicitacao") %>%
#    mutate(cdv=factor(cdv))

#' escrevendo erros_completos
#write.csv(erros_completos,file = "metro_tratado.csv")

#' lendo erros_completos
primeira_palavra <- function(vetor){
    tempo <- NULL
    for(i in vetor){
        tempo <- c(tempo, strsplit(i," ")[[1]][1])
    }
    return(tempo)
}
Metrô <- read.csv(file = "metro_tratado.csv",)[2:10]%>%
    mutate(tempo_abertura=strptime(tempo_abertura, "%Y-%m-%d %H:%M:%S"),
           tempo_falha=strptime(tempo_falha,  "%Y-%m-%d %H:%M:%S"),
           tempo_encerramento=strptime(tempo_encerramento,  "%Y-%m-%d %H:%M:%S"),
           tempo_duracao=duration(primeira_palavra(tempo_duracao))
    )
Metrô_tratado %>%
    filter(tempo_abertura$year==113)
Metrô_tratado$tempo_abertura[1]$year
filtra.ano <- function(vetor, ano){
    resp <- NULL
    for(i in vetor){
        resp <-c(resp,i$year)
    }
}
a <- "cmt"
Metrô_tratado[[a]][1]
str(Metrô_tratado$tempo_abertura[1])
Metrô_tratado %>% select(cdv)
timeS <- Metrô_tratado %>%
    filtra(cdv = "2N03T")

d1 <- min(Metrô$tempo_abertura)
d2 <- max(Metrô$tempo_abertura)

for (i in seq(d1,d2,by = "day")){
    a<-Metrô %>%
        filter(tempo_abertura == as.POSIXlt(i, origin = "1970-01-01")) %>%
        select(.)
    print(a)
}
x <- seq(d1,d2,by = "day")[[1]]
xx<- as.POSIXlt(x, origin = "1970-01-01")

mesmo.dia <- function(.data, dia){
    .data %>%
        filter(tempo_abertura$year == dia$year & tempo_abertura$yday == dia$yday)
}



interval()
timeRange(timeS$tempo_abertura, timeS$tempo_duracao)
dat$time
a <- seq(as.Date(min(Metrô_tratado$tempo_abertura)),as.Date(max(Metrô_tratado$tempo_abertura)),by="days")[1]
Metrô_tratado %>%
    group_by(tempo_abertura$year)%>%
    count(tempo_abertura)
gb$tempo_abertura
Metrô_tratado$tempo_abertura[which(Metrô_tratado$tempo_abertura == "2013-06-15")]
energy$Date <- as.Date(energy$Datetime)
aggregate(Metrô_tratado, by=Metrô_tratado, c)

round.Date(Metrô_tratado$tempo_abertura, "day")
Metrô_tratado$tempo_abertura[which(Metrô_tratado$tempo_abertura == "2013-06-15")]
a <- as.POSIXlt(a)
Metrô_tratado$tempo_abertura[which(Metrô_tratado$tempo_abertura == a)]

x <- c(NA,2,NA,3,4,NA,NA,5,6,7,8,9,10,11,12,13,14)
k <- ts(x)

kei <- function(.data, cdv=NULL){
    if(is.null(cdv)){
        series.temporais(Metrô)
    }else{
        series.temporais(Metrô %>%
            filtra.cdv(cdv)
        )
    }
}
f <- function(cdv = NULL){
    k <- Metrô %>% kei(cdv)
    print()
    serie1 <-ts(k$Qnt, start = c(2013,6,15), freq=365)
    plot(serie1, ylab = "quantidade", xlab = "data da ocorrencia")
}





f(c("2N09T"))


