#' Matriz de termos e documentos
#' 
#' @param s vetor de chr
#' @param tipo tipo de matriz (contagem ou flag)
#' @return uma matriz de termos e documentos
#' 
#' @export
mtd <- function(s, tipo = "contagem"){
  
  p <- stringr::str_split(s, "[:space:]")
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