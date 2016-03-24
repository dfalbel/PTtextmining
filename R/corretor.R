#' Tranformar Corrigir
#' 
#' @family transf
#' Usa o Hunspell p/ corrigir o texto.
#' @note o Hunspell devolve um número de sugestões de correções. O código atual usa a sugestão que mais apareceu no
#' vetor e em caso de empate usa a ordem do Hunspell. Não sei se esse é o melhor jeito.
#' @param s vetor de chr
#' @return vetor com as palavras corrigidas
#' @seealso \code{\link[hunspell]{hunspell_check}} and 
#' \code{\link[hunspell]{hunspell_suggest}}
#'
#' @export
transformar_corrigir <- function(s, dict = system.file("dic/Portuguese (Brazilian).dic", package = "PTtextmining")){
  
  # todo - usar essas palavras sem ser do corpus. talvez de um monte de textos
  # que já venham dentro do pacote
  palavras <- hunspell::hunspell_parse(s) %>% unlist() %>%
    dplyr::data_frame(palavra = .) %>%
    dplyr::group_by(palavra) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(correta = hunspell::hunspell_check(palavra, dict = dict))
  
  erros <- palavras$palavra[!palavras$correta]
  
  sugestoes <- hunspell::hunspell_suggest(words = erros, dict = dict)
  names(sugestoes) <- erros
  sugestoes <- stack(sugestoes) %>%
    dplyr::mutate(ind = as.character(ind)) %>%
    dplyr::left_join(
      palavras %>% dplyr::filter(correta) %>% dplyr::select(-correta), 
      by = c("values" = "palavra")
      ) %>%
    dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>%
    dplyr::group_by(ind) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::summarise(subst = first(values))
    
  substituir_palavras(s, sugestoes$ind, sugestoes$subst)
}
