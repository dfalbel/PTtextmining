#' Matriz de termos e documentos
#' 
#' @param s vetor de chr
#' @param n n usado no n-gram. Esse argumento é usado se você quiser ao invés de
#' uma matriz de termos, uma matriz com os n-grams.
#' @param tipo tipo de matriz (contagem ou flag)
#' @return uma matriz de termos e documentos
#' 
#' @export
mtd <- function(s, n = 1, tipo = "contagem"){
  
  p <- ngram(s, n = n)
  names(p) <- 1:length(p)
  
  m <- plyr::ldply(p, function(c){
    dplyr::data_frame(
      palavra = c
    ) %>%
      dplyr::group_by(palavra) %>%
      dplyr::summarise(n = n())
  }, 
  .id = ".id") %>%
    tidyr::spread(palavra, n, fill = 0)
  
  if(tipo == "flag"){
    m <- m %>% 
      dplyr::mutate_each(dplyr::funs(maior_que_zero_em), -1)
  }
  
  return(m)
}

#' NGRAM
#' 
#' @param s vetor de chr
#' @param n n do n-gram (quantas palavras juntas)
#'
#' @return uma lista com os n-grams para cada elemento do vetor.
#' @seealso \code{\link{mtd}}
#'
#' @export
ngram <- function(s, n = 1){
  n <- n - 1
  p <- stringr::str_split(s, "[:space:]")
  if(n > 0){
    p <- plyr::llply(p, function(c){
      plyr::laply(1:(length(c) - n), function(i, c){
        paste(c[i + (0:n)], collapse = " ")
      }, c = c)
    })
  }
  return(p)
}