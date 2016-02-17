# transformações

#' Tranformar letras do corpus em minusculo
#'
#' @param vetor de chr
#' @return vetor de chr transformado em minúsculos
#'
#' @export
transf_minusculo <- function(x){
  tolower(x)
}

#' Remover pontuação
#'
#'
#' @param vetor de chr
#' @return vetor de chr com a pontuação removida
#'
#' @export
remove_pontuacao <- function(x){
  stringr::str_replace_all(x, "[:punct:]", "")
}


#' Remover urls 
#'
#' @param vetor de chr
#' @return vetor de chr com urls excluidas
#'
#' @export
remove_url <- function(x){
  stringr::str_replace_all(x, "http[^[:space:]]*", "")
}

