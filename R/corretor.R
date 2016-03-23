# para correção de erros de ortografia vamos utilizar o pacote hunspell.

transformar_corrigir <- function(s){
  
  palavras <- hunspell_parse(s) %>% unlist() %>%
    dplyr::data_frame(palavra = .) %>%
    dplyr::group_by(palavra) %>%
    dplyr::summarise(n = n())
  
  path_dicionario <- system.file("dic/Portuguese (Brazilian).dic", package = "PTtextmining")
  
  erros <- hunspell::hunspell(s, dict = path_dicionario)
  
  sugestoes <- lapply(erros, function(x){
    hunspell::hunspell_suggest(words = x, dict = path_dicionario) %>%
      lapply(function(x){
        x[[1]]
      })
  })
}

# s <- c("meu nome é cachoro chatu", "cachorro quenti")
# hunspell_suggest("vc", dict = path_dicionario)
# # hunspell_check("selvagem", dict = "inst/dic/Portuguese (Brazilian).dic")
# 
