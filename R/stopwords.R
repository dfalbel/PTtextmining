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
                     "algures", "alem", "ali", 
                     "amanha", "antes", "aqui", "assim", "atras",
                     "bem", "breve",
                     "cedo", "certamente",
                     "debalde", "depois",
                     "efetivamente", "enfim",
                     "hoje",
                     "mal", "mais", "melhor", "menos", "muito",
                     "nao",
                     "ontem", 
                     "pior", "pouco",
                     "quanto", "quao", "quase",
                     "realmente",
                     "sera", "sim", 
                     "talvez", "tanto")
  
  pt_conjuncoes <- c("e", "nem", "mas", "tambem", "como", "bem", "porem",
                     "todavia", "contudo", "entretanto", "entanto", "ou",
                     "ora", "quer", "ja", "logo", "portanto", "por", "assim",
                     "conseguinte", "que", "porque", "porquanto", "pois",
                     "sendo", "visto", "como", "tal", "tao", "tanto", 
                     "assim", "conforme", "segundo", "consoante", "mesmo",
                     "mais", "ainda", "se", "bem", "embora", "se", "caso",
                     "contanto", "salvo", "medida", "quanto", "fim",
                     "quando", "enquanto", "sempre", "depois")
  
  pt_preposicoes <- c("a", "ante", "apos", "ate", "com", "contra", "de", 
                      "desde", "para", "per", "perante", "por", "sem", "sob", 
                      "sobre", "tras")
  
  pt_pronomes    <- c("algo", "alguem", "algum", "alguns",
                      "cada", "cujo", 
                      "muitos",
                      "nada", "nenhum", "nenhuns", "ninguem",
                      "outrem", "outros",
                      "poucos",
                      "quaisquer", "qualquer", "quantos", "quem",
                      "tantos", "todos", "tudo", 'que', 'nao', 'para',
                      "varios")
  
  pt_stopwords <- c("de", "a", "o", "que", "e", "do", "da", "em", "um", "para", 
                    "com", "não", "uma", "os", "no", "se", "na", "por", "mais", "as", 
                    "dos", "como", "mas", "ao", "ele", "das", "à", "seu", "sua", 
                    "ou", "quando", "muito", "nos", "já", "eu", "também", "só", "pelo", 
                    "pela", "até", "isso", "ela", "entre", "depois", "sem", "mesmo", 
                    "aos", "seus", "quem", "nas", "me", "esse", "eles", "você", "essa", 
                    "num", "nem", "suas", "meu", "às", "minha", "numa", "pelos", 
                    "elas", "qual", "nós", "lhe", "deles", "essas", "esses", "pelas", 
                    "este", "dele", "tu", "te", "vocês", "vos", "lhes", "meus", "minhas", 
                    "teu", "tua", "teus", "tuas", "nosso", "nossa", "nossos", "nossas", 
                    "dela", "delas", "esta", "estes", "estas", "aquele", "aquela", 
                    "aqueles", "aquelas", "isto", "aquilo", "estou", "está", "estamos", 
                    "estão", "estive", "esteve", "estivemos", "estiveram", "estava", 
                    "estávamos", "estavam", "estivera", "estivéramos", "esteja", 
                    "estejamos", "estejam", "estivesse", "estivéssemos", "estivessem", 
                    "estiver", "estivermos", "estiverem", "hei", "há", "havemos", 
                    "hão", "houve", "houvemos", "houveram", "houvera", "houvéramos", 
                    "haja", "hajamos", "hajam", "houvesse", "houvéssemos", "houvessem", 
                    "houver", "houvermos", "houverem", "houverei", "houverá", "houveremos", 
                    "houverão", "houveria", "houveríamos", "houveriam", "sou", "somos", 
                    "são", "era", "éramos", "eram", "fui", "foi", "fomos", "foram", 
                    "fora", "fôramos", "seja", "sejamos", "sejam", "fosse", "fôssemos", 
                    "fossem", "for", "formos", "forem", "serei", "será", "seremos", 
                    "serão", "seria", "seríamos", "seriam", "tenho", "tem", "temos", 
                    "tém", "tinha", "tínhamos", "tinham", "tive", "teve", "tivemos", 
                    "tiveram", "tivera", "tivéramos", "tenha", "tenhamos", "tenham", 
                    "tivesse", "tivéssemos", "tivessem", "tiver", "tivermos", "tiverem", 
                    "terei", "terá", "teremos", "terão", "teria", "teríamos", "teriam")
  
  unique(c(pt_adverbios, pt_conjuncoes, pt_preposicoes, pt_pronomes, pt_stopwords))
}