library(stringr)
library(PTtextmining)
context("Transformações")

# ---------------------

str <- c("ABCD", "ADCEDF")
str_e <- c("abcd", "adcedf")

test_that("transformar minúsculo , transforma em minúsculo", {
  expect_equal(transformar_minusculo(str), str_e)
})

# ---------------------

str <- c("oi!!?", "tudo bem..", "tchau,", "!sexy;:", "!?:;.,")
str_e <- c("oi   ", "tudo bem  ", "tchau ", " sexy  ", "      ")

test_that("remover pontuacao", {
  expect_equal(remover_pontuacao(str), str_e)
})

# ---------------------

str <- c("http://r-pkgs.had.co.nz/tests.html", "https://github.com/ tudo bem..")
str_e <- c(" ", " tudo bem..")

test_that("remover url", {
  expect_equal(remover_url(str), str_e)
})

# -----------------------

str <- c("áéíóúàèìòùãõâêîôûïüñäö")
str_e <- c("aeiouaeiouaoaeiouiunao")

test_that("remover acentos", {
  expect_equal(remover_acentos(str), str_e)
})
