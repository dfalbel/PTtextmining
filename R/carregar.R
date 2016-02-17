# Funções usadas para carregar dados.

#' Carregar dados
#'
#' @param x vetor de chr com textos que deseja carregar
#' @return corpus com os textos
#'
#' @export
carregar_textos <- function(x){
  tm::Corpus(
    tm::VectorSource(x),
    readerControl = list(language = "pt")
  )
}
