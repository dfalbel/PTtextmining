# Lendo Hunspell Affix files

# keywords <- list(
#   "SET",
#   "MAP",
#   "BREAK",
#   "REP",
#   "SFX",
#   "PFX"
# )
# 
# aff <- readLines("data-raw/dicionario/dicionario.aff")
# dic <- readLines("data-raw/dicionario/dicionario.dic")


#' Procurar os sufixos em um dicionário
#'
#' @param aff dicionário de sufixos e afixos no estilo do do hunspell.
#'
look_SFX <- function(aff){
  
  grupos <- dplyr::data_frame(
    indice = which(stringr::str_detect(aff, "SFX [:alpha:] Y [:digit:]")),
    nome = aff[indice]
  ) %>%
    tidyr::separate(nome, c("SFX", "tag", "Y", "qtd"))
  
  sufixos <- plyr::adply(grupos, 1, function(x){
    dplyr::data_frame(
      SFX = aff[x$indice + 1:x$qtd]
    )
  }) %>%
    dplyr::select(-indice, -tag, -Y, -qtd) %>%
    tidyr::separate(SFX, c("SFX", "tag", "remover", "adicionar", "procurar"), "[:space:]+")
  
}

#' Processar o dicionario
#'
#' @param dic dicionário no estilo do dicionario do hunspell
#'
process_dic <- function(dic){
  dicionario <- dplyr::data_frame(
    palavras = dic[-1]
  ) %>%
    tidyr::separate(
      palavras,
      c("palavras", "tag"),
      "/",
      fill = "right"
    )
 return(dicionario) 
}

# As <- dicionario %>% filter(str_detect(tag, "A"))
# sA <- sufixos %>% filter(tag == "A")
# sA <- sA[1,]
# As %>%
#   filter(str_detect(palavras, "ão\\b")) %>%
#   mutate(palavra2 = str_replace_all(palavras, "o\\b", "es")) %>%
#   View



