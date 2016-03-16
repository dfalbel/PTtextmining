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
remover_url <- function(s){
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

#' Remover acentos
#'
#' @param s vetor de chr
#' @return vetor de chr com os acentos removidos
#'
#' @export
remover_acentos <- function(s){
  chartr("áéíóúàèìòùãõâêîôûïüñäö",
         "aeiouaeiouaoaeiouiunao", s)
}

#' Substituir uma palavra por outra
#'
#' @param s vetor de chr
#' @param pa palavra que deve ser substituida
#' @param pd palavra colocada no lugar de pa
#' @return vetor de chr com as palavras substituidas
#'
#' @export
substituir_palavra <- function(s, pa, pd){
  stringr::str_replace_all(s, paste0("\\b", pa, "\\b"), pd)
}

#' Substituir uma lista de palavras por uma lista de palavras
#'
#' @param s vetor de chr
#' @param pa vetor de palavras que serão substituidas
#' @param pd vetor de palavras que serão colcoadas no lugar
#' @return vetor de chr com as palavras substituidas
#'
#' @export
substituir_palavras <- function(s, pa, pd){
  for(i in 1:length(pa)){
    s <- substituir_palavra(s, pa[i], pd[i])
  }
  s
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
  substituir_palavra(s, p, "")
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

#' Remover Stopbords
#'
#' @param s vetor de chr 
#' @return vetor de chr sem as stopwords
#' 
#' @seealso \code{\link{stopwords}}
#' 
#' @export
remover_stopwords <- function(s, ignorar = character(0)){
  p <- stopwords()[!stopwords() %in% ignorar]
  remover_palavras(s, p)
}

#' Fazer o Stemming
#' 
#' Essa função usa a função stemDocument do pacote tm para realizar o stemming.
#' Por sua vez, o tm utiliza o algoritmo Porter para realizar o stemming. Esse 
#' algoritmo é descrito no seguinte link: 
#' http://snowball.tartarus.org/algorithms/portuguese/stemmer.html 
#' 
#' @param s vetor de chr
#' @return vetor de chr com stemming realizado
#'
#' @export
transformar_stemming <- function(s){
  
  palavras <- dplyr::data_frame(
    antes = stringr::str_split(s, " ") %>% unlist
  ) %>%
    dplyr::group_by(antes) %>%
    dplyr::summarise(n_antes = n()) %>%
    dplyr::mutate(
      depois = tm::stemDocument(antes, language = "pt")
    )
  
  # tirar as palavras que não são usadas
  palavras <- palavras %>%
    dplyr::filter(antes != depois) %>% # só substituir se antes for != de depois
    dplyr::filter(!is.null(antes), !is.null(depois)) %>% # só substituir se não for NULL
    dplyr::filter(antes != "", depois != "") %>% # 
    dplyr::filter(antes != " ", depois != " ") %>%
    dplyr::group_by(depois) %>%
    dplyr::mutate(n_depois = n()) %>%
    dplyr::filter(n_depois > 1) %>%
    dplyr::ungroup()
  
  # usar a palavra que mais aparece antes
  palavras <- palavras %>%
    dplyr::group_by(depois) %>%
    dplyr::arrange(desc(n_antes)) %>%
    dplyr::mutate(subst = first(antes))
    
  if(nrow(palavras) > 0){
    s <- substituir_palavras(s, palavras$antes, palavras$subst) 
  }
  
  return(s)
}


#' Encontrar erros
#' 
#' @param s vetor de chr
#' @return vetor com as palavras que estão escritas erradas o texto.
#'
encontrar_erros <- function(s){
  p <- stringr::str_extract_all(s, "\\b[:alpha:]+\\b") %>%
    unlist() %>%
    unique()
  check <- hunspell::hunspell_check_pt(p)
  
  p <- p[!check]
  
  return(p)
}


#' Corrigir as palavras
#' 
#' @param s vetor de chr
#' @return vetor de chr com palavras corrigidas usando o hunspell
#' 
#' @seealso \code{\link[hunspell]{hunspell_check_pt}} and 
#' \code{\link[hunspell]{hunspell_suggest_pt}}
#' 
#' @export
transformar_corrigir <- function(s){
  
  p <- encontrar_erros(s)
  
  if(length(p) > 0){
    palavras <- dplyr::data_frame(
      antes = p,
      depois = hunspell::hunspell_suggest_pt(p) %>%
        plyr::laply(function(x){
          x[[1]]
        })
    )
    palavras <- palavras %>% 
      dplyr::filter(!is.null(depois))
    
    s <- substituir_palavras(s, palavras$antes, palavras$depois)
  }
  return(s)
}

#' Remover palavras erradas
#' 
#' @param s vetor de chr
#' @return vetor de chr com as palavras escritas de forma errada removidas.
#' 
#' @export
remover_palavras_erradas <- function(s){
  p <- encontrar_erros(s)
  s <- remover_palavras(s, p)
  return(s)
}
