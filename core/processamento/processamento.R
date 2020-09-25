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
save(list = c("segrega.cdv","calcula.cdv"), file = "funcoes/processamento.RData")
