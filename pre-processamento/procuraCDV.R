magia_negra <- function(descricao, cmt="HACK"){
    print("*")
    if(is.null(descricao)){
        return(NULL)
    }
    d <- str_replace_all(descricao,pattern = "\\s+",replacement = "")
    ids <- str_extract_all(string = d,pattern="[1-3](\\-|\\.|\\:|\\;|\\,)*[S|N](\\-|\\.|\\:|\\;|\\,)*[01](\\-|\\.|\\:|\\;|\\,)?[0-9](\\-|\\.|\\:|\\;|\\,)?($|T$|T[^T])")
    print(ids)
    return(ids[[1]])
}
conjura <- function(dados){
    cdvs <- list()
    desc <- list()
    k <- 1
    for(i in 1:nrow(dados)){
        aux <- magia_negra(dados$descricao[i])
        if(!is.null(aux) & length(aux)!=0){
            x <- NULL
            for(a in aux){
                if(grepl("T[^T]$",a)){
                    a <- substr(a,1,(nchar(a)-1))
                }else if(grepl("[^T]$",a)){
                    a <- paste0(a, "T")
                }
                a <- str_replace_all(a,"(\\.|\\,|\\-|\\:|\\;)","")
                x <- c(x,a)
            }
            cdvs[k] <- list(cdvs=x)
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

save(list = c("magia_negra","conjura","erro.cdvs"), file = "funcoes/magia.RData")