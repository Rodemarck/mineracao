rm(list = ls())#
load("funções/ler.RData")
load("funções/constantes.RData")
load("funções/magia.RData")

LOG <- function(..., tipo="log de ") {
    message(tipo,sys.call(-1),"\n",...)
    traceback()
}
save(list = c(ls()),file = "funções/import.RDAta")
message("eita=",15,"deu ruim")

