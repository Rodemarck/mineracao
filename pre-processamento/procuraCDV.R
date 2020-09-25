magia_negra <- function(descricao, cmt="HACK"){
    if(is.null(descricao)){
        return(NULL)
    }
    d <- str_replace_all(descricao,pattern = "\\s+",replacement = "")
    ids <- str_extract_all(string = d,pattern="[1-3](\\-|\\.|\\:|\\;|\\,)*[S|N](\\-|\\.|\\:|\\;|\\,)*[01](\\-|\\.|\\:|\\;|\\,)?[0-9](\\-|\\.|\\:|\\;|\\,)?($|T$|T[^T])")
    return(ids[[1]])
}
conjura <- function(dados){
    df <- data.frame(solicitacao=character(),cdv=character())

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
            for(cdv in x){
                df[k,] <- c(dados$solicitacao[i],cdv)

                k+1 -> k
            }
        }
    }
    return(df)
}

save(list = c("magia_negra","conjura","erro.cdvs"), file = "funcoes/magia.RData")