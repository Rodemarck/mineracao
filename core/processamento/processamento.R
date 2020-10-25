#' Divide os as solicitações relatorios de erros de acordo com
#' os cdvs
#'
#' @author Rodemarck
#' @param erros lista contendo
segrega.cdv <- function(erros){
    df <- list()
    for(i in 1:length(erros$Cdvs)){
        for(e in erros$Cdvs[[i]]){
            n <- length(df[[e]])+1
            df[[e]][n] <- erros$Desc[[i]][1]
        }
    }
    return(df)
}
calcula.cdv <- function(resultados, Metrô){
    df <- list()
    for(r in names(resultados)){
        if(length(resultado[[r]]) < 2){
            df[[r]] <- list(MTTR=Metrô$tempo_duracao[which(Metrô$solicitacao == resultado[[r]][[1]])], MTTF=NA)
        }else{
            tr <- Metrô$tempo_duracao[which(Metrô$solicitacao == resultado[[r]][[1]])]
            tf <- NA
            for(i in 2:length(resultado[[r]])){
                tr <- c(tr,Metrô$tempo_duracao[which(Metrô$solicitacao == resultado[[r]][[i]])])
                tf <- c(tf,(
                        Metrô$tempo_abertura[which(Metrô$solicitacao == resultado[[r]][[i]])]
                        - Metrô$tempo_encerramento[which(Metrô$solicitacao == resultado[[r]][[i-1]])]))
            }
            df[[r]] <- list(Tr=tr,Tf=tf)
        }
    }
    return(df)
}
filtra.cdv <- function(.data, cdv=NA){
    resultado <- NULL
    for(i in .data$cdv){
        resultado <- c(resultado, grepl(paste0(".*(",paste(cdv,collapse = "|"),").*"),i))
    }
    return(.data %>% filter(resultado))
}
mesmo.dia <- function(.data, dia){
    .data %>%
        filter(tempo_abertura$year == dia$year & tempo_abertura$yday == dia$yday)
}
series.temporais <- function (.data){
    print(nrow(.data))
    #st <- data.frame("quantidade"=numeric(),"solicitacoes"=character(), stringsAsFactors = F)
    qt <- NULL
    st <- NULL
    d1 <- min(.data$tempo_abertura)
    d2 <- max(.data$tempo_abertura)
    k <- 1
    for (i in seq(d1,d2,by = "day")){
        a<-Metrô %>%
            mesmo.dia(dia=as.POSIXlt(i, origin = "1970-01-01"))
        if(nrow(a) !=0){
            soli <- a %>%
                select(solicitacao)
            n <-nrow(a)
            soli <- unlist(soli)
            soli <- unique(soli)
            a <- paste(soli,collapse = "-")
            qt <- c(qt,as.character(n))
            st <- c(st,as.character(a))
        }
        else{
            qt <- c(qt, NA)
            st <- c(st, NA)
        }
        
        k+1 -> k
    }
    return(data.frame("Qnt"=qt, "Soli"=st, stringsAsFactors = F))
}

save(list = c("segrega.cdv","calcula.cdv","filtra.cdv","series.temporais","mesmo.dia"), file = "funcoes/processamento.RData")
