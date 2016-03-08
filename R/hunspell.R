# Lendo Hunspell Affix files

keywords <- list(
  "SET",
  "MAP",
  "BREAK",
  "REP",
  "SFX",
  "PFX"
)

aff <- readLines("data-raw/dicionario/dicionario.aff")

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
    tidyr::separate(SFX, c("SFX", "tag", "aux", "subst", "regex"), "[:space:]+")
  
}