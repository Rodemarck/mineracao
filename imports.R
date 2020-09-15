rm(list = ls())#
for(f in list.files("funcoes")){
    load(paste0("funcoes/",f))
}
rm(f)
save(list = c(ls()),file = "funcoes/import.RData")

