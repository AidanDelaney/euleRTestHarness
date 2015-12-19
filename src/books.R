library("euleR")
library("venneuler")

library("tm")

download.file("http://www.gutenberg.org/cache/epub/4300/pg4300.txt", "ulysses.txt")
download.file("http://www.gutenberg.org/cache/epub/10/pg10.txt", "bible.txt")
download.file("http://www.gutenberg.org/cache/epub/174/pg174.txt", "DorianGray.txt")
download.file("http://www.gutenberg.org/cache/epub/550/pg550.txt", "SilasMarner.txt")
download.file("http://www.gutenberg.org/cache/epub/2701/pg2701.txt", "MobyDick.txt")
download.file("http://www.gutenberg.org/cache/epub/2264/pg2264.txt", "Macbeth.txt")

fileToWordList <- function(file) {
  corpus <- VCorpus(DirSource(pattern=file))
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  # Get the words from Ulysees as a list
  words <- colnames(as.matrix(DocumentTermMatrix(corpus)))
}

ulysses <- fileToWordList(file="ulysses.txt")
bible <- fileToWordList(file="bible.txt")
dorian_gray <- fileToWordList(file="DorianGray.txt")
silas_marner <- fileToWordList("SilasMarner.txt")
moby_dick <- fileToWordList("MobyDick.txt")
macbeth <- fileToWordList("Macbeth.txt")

z_u_b = length(setdiff(ulysses, bible))
z_ub  = length(intersect(ulysses, bible))
z_b_u = length(setdiff(bible, ulysses))

print(length(ulysses))
print(z_u_b)
print(z_ub)
print(z_b_u)