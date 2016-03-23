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

#' Detectar palavra
#' 
#' @param s vetor de chr
#' @param p palavra que deseja detectar
#'
#' @export
detectar <- function(s, p){
  stringr::str_detect(s, paste0("\\b", p, "\\b", collapse = ""))
}

#' Palavras seguidas
#' 
#' Quais são as palavras que mais aparecem antes ou depois da palavra p.
#' @param s vetor de chr
#' @param p palavra que deseja buscar
#' @param qtd quantidade de palavras que você quer mostrar (se = NULL, mostrar 
#' todos)
#'
#' @return um data.frame com as colunas ng_a palavra que mais veio antes e n_a 
#' a quantidade de vezes (qtd). ng_d são as palavras que aparecem depois do termo p.
#'
#' @export
palavras_seguidas <- function(s, p, qtd = NULL){
  
  ng <- ngram(s, n = 2) %>% unlist
  ng <- ng[detectar(ng, p)]
  
  ng_depois <- ng[stringr::str_detect(ng, paste0("\\b", p, " ", collapse = ""))]
  ng_antes <- ng[stringr::str_detect(ng, paste0(" ", p, "\\b", collapse = ""))]
  
  depois <- dplyr::data_frame(ng_d = ng_depois) %>%
    dplyr::group_by(ng_d) %>%
    dplyr::summarise(n_d = n()) %>%
    dplyr::arrange(desc(n_d)) %>%
    dplyr::mutate(ng_d = ng_d %>% remover_palavra(p) %>% 
                    remover_espacos_excedentes()
                  )
  
  antes <- dplyr::data_frame(ng_a = ng_antes) %>%
    dplyr::group_by(ng_a) %>%
    dplyr::summarise(n_a = n()) %>%
    dplyr::arrange(desc(n_a)) %>%
    dplyr::mutate(ng_a = ng_a %>% remover_palavra(p) %>% 
                    remover_espacos_excedentes()
                  )
  
  r <- dplyr::bind_cols(
    antes, depois
  )
  if(!is.null(qtd)){
    dplyr::slice(1:qtd)
  }
  return(r)
}
