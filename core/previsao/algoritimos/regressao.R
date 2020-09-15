# Title     : TODO
# Objective : TODO
# Created by: rodemarck
# Created on: 13/09/2020
calcula_reta_simples <- function (lista,param){
    produto <- lista * param
    lista_quadrado <- lista^2
    parametros_quadrados <- param^2
    arametro_quadrado_sum <- sum(parametro_quadrado)
    lista_quadrado_sum <- sum(lista_quadrado)
    produtos_sum <- sum(produtos)
    lista_sum <- sum(lista)
    n <- length(lista)

  a <- (produtos_sum- (lista_sum * produtos_sum)) / lista_quadrado_sum - (lista_sum ^ 2)
  b = mean(parametros) - (a * mean(lista))
  r = n * produtos_sum - (lista_sum * produtos_sum) / sqrt((n * lista_quadrado_sum - lista_sum ** 2) *  (n * lista_quadrado_sum - produtos_sum ** 2))
  return (tuple(a,b,r))
}


