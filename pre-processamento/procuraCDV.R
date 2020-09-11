magia_negra <- function(descricao, cmt="HACK"){
    if(is.null(descricao)){
        return(NULL)
    }
    d <- str_replace_all(descricao,pattern = "\\s+",replacement = "")
    ids <- str_extract_all(string = d,pattern="[1-3](\\-|\\.|\\:|\\;|\\,)*[S|N](\\-|\\.|\\:|\\;|\\,)*[01](\\-|\\.|\\:|\\;|\\,)?[0-9](\\-|\\.|\\:|\\;|\\,)?($|T$|T[^T])")
    return(ids)
}
conjura <- function(dados){
    cdvs <- list()
    desc <- list()
    k <- 1
    for(i in 1:nrow(dados)){
        aux <- magia_negra(dados$descrição[i])
        if(!is.null(aux)){
            print(aux)
        }
        if(!is.null(aux) & length(aux[[1]])!=0){
            print(aux[[1]])
            cdvs[k] <- list(cdvs=aux[[1]])
            desc[k] <- list(dados[i,])
            
            k+1 -> k
        }
    }
    return(list(Cdvs=cdvs, Desc=desc))
}
erro.cdvs <- function(erros){
    x <- list()
    1 -> n
    for(i in 1:length(erros$Cdvs)){
        if(cdv %in% erros$Cdvs[[i]]){
            x[n] <- list(erros$Desc[[i]])
            n+1 -> n
        }
    }
    return(x)
}

save(list = c("magia_negra","conjura","erro.cdvs"), file = "funcões/magia.RData")