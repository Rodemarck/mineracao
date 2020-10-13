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
        resultado <- c(resultado, grepl(paste0(".*",cdv,".*"),i))
    }
    return(.data %>% filter(resultado))
}
series.temporais <- function (.data){
    st <- data.frame("quantidade","solicitacoes", stringsAsFactors = F)
    mesmo.dia <- function(.data, dia){
        .data %>%
            filter(tempo_abertura$year == dia$year & tempo_abertura$yday == dia$yday)
    }
    d1 <- min(.data$tempo_abertura)
    d2 <- max(.data$tempo_abertura)
    k <- 1
    for (i in seq(d1,d2,by = "day")){
        a<-Metrô %>%
            filter(tempo_abertura == as.POSIXlt(i, origin = "1970-01-01"))
        if(nrow(a) ==0){
        
        }
    }
}

save(list = c("segrega.cdv","calcula.cdv","filtra.cdv"), file = "funcoes/processamento.RData")
