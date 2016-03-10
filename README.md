<!-- README.md is generated from README.Rmd. Please edit that file -->
Text Mining PT
==============

Este pacote implementa uma série de funções que auxiliam no tratamento de textos escritos em português para que possam ser usados em análises de Text Mining.

Uma das principais diferenças deste pacote é que ele é feito para lidar com vetores de caracteres e não com `Corpus` que nem o pacote `tm`. Isso o torna muito mais simples de utilizar mesmo que exista queda na performance.

Usando
======

Considerando o seguinte parágrafo:

``` r
s <- c("Já dizia o clichê: “dados são o novo ouro”. O mundo gera informação esponencial e ao mesmo tempo, todos querem uma fatia desse bolo. Intuição ou regras do senso comum são úteis, mas não suficientes. É preciso saber que os dados permitem às empresas e organizações entenderem seus clientes, produtos e processos muito melhor.")
```

Ele possui um erro de ortografia: na palavra excencial que está escrita essencial Além disso, o texto precisa de um tratamento, antes de entrar em algum algoritmo de text mining.

Com o pacote, é possível fazer da seguinte maneira:

``` r
library(PTtextmining)
s %>%
  transformar_minusculo() %>%
  transformar_corrigir() %>%
  remover_stopwords() %>%
  remover_acentos() %>%
  remover_pontuacao() %>%
  remover_numeros() %>%
  remover_dinheiro() %>%
  remover_espacos_excedentes()
#> [1] "dizia cliche dados novo ouro mundo gera informaçao exponencial tempo querem fatia desse bolo intuiçao regras senso comum uteis suficientes e preciso saber dados permitem empresas organizaçoes entenderem clientes produtos processos"
```
