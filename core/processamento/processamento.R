# Title     : TODO
# Objective : TODO
# Created by: rodemarck
# Created on: 13/09/2020
processa <- function(erros){
    df <- list()
    for(i in 1:length(erros$Cdvs)){
        for(e in erros$Cdvs[[i]]){
            n <- length(df[[e]])+1
            df[[e]][n] <- erros$Desc[[i]][1]
        }
    }
    return(df)
}
save(processa, file = "funcoes/processamento.RData")
