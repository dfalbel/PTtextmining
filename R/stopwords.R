#' Stopwords
#' 
#' Palavras que geralemente não trazem informações relevantes para análise dos 
#' textos.
#' 
#' @return vetor com palavras consideradas stopwords
#' 
#' @export
stopwords <- function(){
  pt_adverbios <-  c("acaso", "acinte", 
                     "adiante", "adrede",
                     "afinal", "afora", "agora",
                     "algures", "além", "ali", 
                     "amanhã", "antes", "aqui", "assim", "atrás",
                     "bem", "breve",
                     "cedo", "certamente",
                     "debalde", "depois",
                     "efetivamente", "enfim",
                     "hoje",
                     "mal", "mais", "melhor", "menos", "muito",
                     "não",
                     "ontem", 
                     "pior", "pouco",
                     "quanto", "quão", "quase",
                     "realmente",
                     "será", "sim", 
                     "talvez", "tanto")
  
  pt_conjuncoes <- c("e", "nem", "mas", "também", "como", "bem", "porém",
                     "todavia", "contudo", "entretanto", "entanto", "ou",
                     "ora", "quer", "já", "logo", "portanto", "por", "assim",
                     "conseguinte", "que", "porque", "porquanto", "pois",
                     "sendo", "visto", "como", "tal", "tão", "tanto", 
                     "assim", "conforme", "segundo", "consoante", "mesmo",
                     "mais", "ainda", "se", "bem", "embora", "se", "caso",
                     "contanto", "salvo", "medida", "quanto", "fim",
                     "quando", "enquanto", "sempre", "depois")
  
  pt_preposicoes <- c("a", "ante", "após", "até", "com", "contra", "de", 
                      "desde", "para", "per", "perante", "por", "sem", "sob", 
                      "sobre", "tras")
  
  pt_pronomes    <- c("algo", "alguém", "algum", "alguns",
                      "cada", "cujo", 
                      "muitos",
                      "nada", "nenhum", "nenhuns", "ninguém",
                      "outrem", "outros",
                      "poucos",
                      "quaisquer", "qualquer", "quantos", "quem",
                      "tantos", "todos", "tudo", 'que', 'não', 'para',
                      "vários")
  
  pt_stopwords <- c("de", "a", "o", "que", "e", "do", "da", "em", "um", "para", 
                    "com", "não", "uma", "os", "no", "se", "na", "por", "mais", "as", 
                    "dos", "como", "mas", "ao", "ele", "das", "a", "seu", "sua", 
                    "ou", "quando", "muito", "nos", "já", "eu", "também", "só", "pelo", 
                    "pela", "até", "isso", "ela", "entre", "depois", "sem", "mesmo", 
                    "aos", "seus", "quem", "nas", "me", "esse", "eles", "você", "essa", 
                    "num", "nem", "suas", "meu", "as", "minha", "numa", "pelos", 
                    "elas", "qual", "nos", "lhe", "deles", "essas", "esses", "pelas", 
                    "este", "dele", "tu", "te", "vocês", "vós", "lhes", "meus", "minhas", 
                    "teu", "tua", "teus", "tuas", "nosso", "nossa", "nossos", "nossas", 
                    "dela", "delas", "esta", "estes", "estas", "aquele", "aquela", 
                    "aqueles", "aquelas", "isto", "aquilo", "estou", "esta", "estamos", 
                    "estão", "estive", "esteve", "estivemos", "estiveram", "estava", 
                    "estávamos", "estavam", "estivera", "estivéramos", "esteja", 
                    "estejamos", "estejam", "estivesse", "estivessemos", "estivessem", 
                    "estiver", "estivermos", "estiverem", "hei", "há", "havemos", 
                    "hão", "houve", "houvemos", "houveram", "houvera", "houvéramos", 
                    "haja", "hajamos", "hajam", "houvesse", "houvessemos", "houvessem", 
                    "houver", "houvermos", "houverem", "houverei", "houverá", "houveremos", 
                    "houverão", "houveria", "houveríamos", "houveriam", "sou", "somos", 
                    "são", "era", "éramos", "eram", "fui", "foi", "fomos", "foram", 
                    "fora", "fôramos", "seja", "sejamos", "sejam", "fosse", "fôssemos", 
                    "fossem", "for", "formos", "forem", "serei", "será", "seremos", 
                    "serão", "seria", "seríamos", "seriam", "tenho", "tem", "temos", 
                    "têm", "tinha", "tínhamos", "tinham", "tive", "teve", "tivemos", 
                    "tiveram", "tivera", "tivéramos", "tenha", "tenhamos", "tenham", 
                    "tivesse", "tivessemos", "tivessem", "tiver", "tivermos", "tiverem", 
                    "terei", "terá", "teremos", "terão", "teria", "teríamos", "teriam")
  
  unique(c(pt_adverbios, pt_conjuncoes, pt_preposicoes, pt_pronomes, pt_stopwords))
}
