# transformações

#' Tranformar letras do corpus em minusculo
#'
#' @param s vetor de chr
#' @return vetor de chr transformado em minúsculos
#'
#' @export
transformar_minusculo <- function(s){
  tolower(s)
}

#' Remover pontuação
#'
#'
#' @param s vetor de chr
#' @return vetor de chr com a pontuação removida
#'
#' @export
remover_pontuacao <- function(s){
  stringr::str_replace_all(s, "[:punct:]", "")
}


#' Remover urls 
#'
#' @param s vetor de chr
#' @return vetor de chr com urls excluidas
#'
#' @export
remover_url <- function(x){
  s <- stringr::str_replace_all(s, "http\\S+\\s*", "")
  stringr::str_replace_all(s, "www.\\S+\\s*", "")
}


#' Remover hashtags
#' 
#' @param s vetor de chr
#' @return vetor de chr com hashtags excluidas
#'  
#' @export     
remover_hashtags <- function(s){
  stringr::str_replace_all(s, "#\\S+\\s*", "")
}

#' Remover dinheiro
#' 
#' @param s vetor de chr
#' @return vetor de chr com r$ excluido
#' 
#' @note deve ser usada depois de transformar o texto em minúsculo.
#' 
#' @export
remover_dinheiro <- function(s){
  stringr::str_replace_all(s, stringr::fixed("r$"), "") 
}

#' Remover números 
#' 
#' @param s vetor de chr
#' @return vetor de chr sem números
#' 
#' @export
remover_numeros <- function(s){
  stringr::str_replace_all(s, "[[0-9]]", "")
}


#' Remover espaços excedentes
#' 
#' @param s vetor de chr
#' @return vetor de chr seme spaços excedentes
#' 
#' @export
remover_espacos_excedentes <- function(s){
  s <- stringr::str_trim(s, "both")
  s <- stringr::str_replace_all(s, "  ", " ")
  s <- stringr::str_replace_all(s, "  ", " ")
  s <- stringr::str_replace_all(s, "  ", " ")
  s <- stringr::str_replace_all(s, "  ", " ")
  s <- stringr::str_replace_all(s, "  ", " ")
  stringr::str_replace_all(s, "  ", " ")
}

#' Remover palavra
#' 
#' @param s vetor de chr
#' @param p palavra que deseja excluir
#' 
#' @return vetor de chr sem a palavra p
#' 
#' @export
remover_palavra <- function(s, p){
  stringr::str_replace_all(s, paste0("\\b", p, "\\b"), "")
}

#' Remover palavras
#' 
#' @param s vetor de chr
#' @param p vetor de palavras que desejar excluir
#' @return vetor de chr sem as palavras indicadas no parametro p
#' 
#' @export
remover_palavras <- function(s, p){
  for(i in 1:length(p)){
    s <- remover_palavra(s, p[i])
  }
  s
}



