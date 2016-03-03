# corretor ortográfico

# Vou tentar usar este código p/ fazer o corretor.
# http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/

# juntar todos arquivos

# dicionário :)
f <- readLines("https://cgit.freedesktop.org/libreoffice/dictionaries/plain/pt_BR/pt_BR.dic")

arq <- list.files("data-raw/livros/machado de assis/", full.names = T)
con <- file("data-raw/machado de assis.txt", open = "w")
for(a in arq){
  texto <- readLines(a)
  writeLines(texto, con)
}
close(con)

# Read in big.txt, a 6.5 mb collection of different English texts.
raw_text <- paste(readLines("data-raw/machado de assis.txt"), collapse = " ")
# Make the text lowercase and split it up creating a huge vector of word tokens.
split_text <- strsplit(tolower(raw_text), "[^a-z]+")
# Count the number of different type of words.
word_count <- table(split_text)
# Sort the words and create an ordered vector with the most common type of words first.
sorted_words <- names(sort(word_count, decreasing = TRUE))

correct <- function(word) {
  # Calculate the edit distance between the word and all other words in sorted_words.
  edit_dist <- adist(word, sorted_words)
  # Calculate the minimum edit distance to find a word that exists in big.txt 
  # with a limit of two edits.
  min_edit_dist <- min(edit_dist, 2)
  # Generate a vector with all words with this minimum edit distance.
  # Since sorted_words is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])
  # In case proposals_by_prob would be empty we append the word to be corrected...
  proposals_by_prob <- c(proposals_by_prob, word)
  # ... and return the first / most probable word in the vector.
  proposals_by_prob[1]
}

sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("http://www.norvig.com/big.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))
correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }