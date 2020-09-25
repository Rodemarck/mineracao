#leitura e formatacao inicial
ler <- function(x){
    Metrô <- as.data.frame(read.csv(x, check.names = F))
    #nomeando colunas
    names(Metrô) <- c("solicitacao","localizacao","data_abertura","hora_abertura","data_falha","hora_falha","descricao","data_encerramento","hora_encerramento")
    #filtragem

    Metrô <-Metrô %>%
                filter(localizacao != "" & localizacao != "Bem/Localiz." & !is.na(localizacao)) %>%
                mutate(tempo_abertura=strptime(paste(data_abertura, hora_abertura, sep = " "), "%m/%d/%Y %H:%M"),
                       tempo_falha=strptime(paste(data_falha, hora_falha, sep = " "), "%m/%d/%Y %H:%M"),
                       tempo_encerramento=strptime(paste(data_encerramento, hora_encerramento, sep = " "), "%m/%d/%Y %H:%M")) %>%
               select(c("solicitacao","localizacao","descricao","tempo_abertura","tempo_falha","tempo_encerramento"))
    Metrô <-Metrô %>%
                mutate(tempo_encerramento=ifelse(is.na(tempo_encerramento),tempo_abertura+median(tempo_encerramento-tempo_abertura, na.rm = T),tempo_encerramento)) %>%
                mutate(tempo_duracao=duration(minute=as.integer(tempo_encerramento-tempo_abertura,units("minutes"))))
    return(Metrô)
}


cdvs <- function(x,a,b){
    if(!x %in% c("RECS","NEV","CAJ")){
        x <- "HACK"
    }
    l <- switch (x,
        "RECS" = list(
            c("1N01T","1S01T","1S02T","1S03T","1S04T","1S05T","1S06T","1S07T","1S08T","1S09T"),
            c("2N02T","2N01T","2S01T","2S02T","2S03T","2S04T","2S05T","2S06T","2S07T","2S08T","2S09T"),
            c(),
        "NEV" = list(
            c("1N14T","1N13T","1N12T","1N11T","1N10T","1N09T","1N08T","1N07T","1N06T","1N05T","1N04T","1N03T","1N02T","1N01T","1S01T","1S02T","1S03T"),
            c("2N14T","2N13T","2N12T","2N11T","2N10T","2N09T","2N08T","2N07T","2N06T","2N05T","2N04T","2N03T","2N02T","2N01T","2S01T","2S02T","2S03T"),
            c("3N03T","3S02T","3S01T")),
        "CAJ" = list(
            c("1N13T","1N12T","1N11T","1N10T","1N09T","1N08T","1N07T","1N06T","1N05T","1N04T","1N03T","1N02T","1N01T"),
            c("2N13T","2N12T","2N11T","2N10T","2N09T","2N08T","2N07T","2N06T","2N05T","2N04T","2N03T","2N02T","2N01T"),
            c()),
        "HACK" = list(
            c("1S08T","1N01T", "1S01T", "1S02T", "1S03T", "1S04T", "1S05T", "1S06T", "1S07T", "1S09T", "1N14T", "1N13T", "1N12T", "1N11T", "1N10T", "1N09T", "1N08T", "1N07T", "1N06T", "1N05T", "1N04T", "1N03T", "1N02T"),
            c("2S08T","2N01T", "2S01T", "2S02T", "2S03T", "2S04T", "2S05T", "2S06T", "1S07T","2S09T", "2N14T", "2N13T", "2N12T", "2N12T", "2N20T", "2N09T", "2N08T", "2N07T", "2N06T", "2N05T", "2N04T", "2N03T", "2N02T"),
            c())
        )
        
    )
    if(is.numeric(a) | a<1 | a>3){
        return(F)
    }
    return(b %in% l[[a]])
}
cmt <- function(x){
    return(switch(x,
                  "RECS"="RECS",
                  "JOAS"="RECS",
                  "PAZ"="RECS",
                  "IMB"="NEV",
                  "FAL"="NEV",
                  "SHO"="NEV",
                  "NEV"="NEV",
                  "PTO"="NEV",
                  "LAR"="CAJ",
                  "GUA"="CAJ",
                  "PRZ"="CAJ",
                  "CAJ"="CAJ")
           )
}
ESTACOES <- c("RECS","JOAS","PAZ","IMB","FAL","SHO","NEV","PTO","LAR","GUA","PRZ","CAJ")
estacao <- function(x){
    if(x %in% ESTACOES){
        return(x)
    }
    if(x %in% c("REC","RECSUL")){
        return("RECS")
    }
    if(x %in% c("JOA","JOASUL")){
        return("JOAS")
    }
    return(NULL)
}
CMTS <- c("RECS","NEV","CAJ")



save(ler,file = ".\\..\\funcoes\\ler.RData")
save(list =c("cdvs","CMTS","cmt","ESTACOES","estacao"), file = ".\\..\\funcoes\\constantes.RData")
