library("euleR")
library("venneuler")

library("tm")

u_book <- "ulysses.txt"
b_book <- "bible.txt"
d_book <- "DorianGray.txt"
s_book <- "SilasMarner.txt"
m_book <- "MobyDick.txt"
a_book <- "Macbeth.txt"

# Top n books (29) from project Gutenberg before you hit the first "repeat"
# i.e. one of the top books is the collected Sherlock Homes stories by Arthur Conan Doyle, where as the 30th most favourite book is "A Study in Scarlet" (excellent TV adaption on BBC!).  This is a repeat of a portion of the collected stories, so we stopped at 29.

#
# TODO: clean up all occurances of the magic number 29 in this file and test.R
# FIXME: cut to 26 as we onlt have 26 built in labels in the letters function.
books = list("1342", "11", "46", "1661", "27827", "2660", "76", "98", "74", "84", "2591", "30254", "1400", "5200", "174", "1952", "135", "345", "1232", "158", "844", "16", "1184", "23", "1322") #, "4300", "2701", "1260", "17135")

checkCache <- function (url, cache_file) {
  if(!file.exists(cache_file)) {
    download.file(url, cache_file)
  }
}

downloadBooks <- function (bs) {
  for(book in bs) {
    url = paste0("http://www.gutenberg.org/cache/epub/", book, "/pg", book, ".txt")
    file = paste0(book, ".txt")
    checkCache(url, file)
  }
}

fileToWordList <- function(file) {
  corpus <- VCorpus(DirSource(directory=".", pattern=file))

  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))

  # Get the words from Ulysees as a list
  words <- colnames(as.matrix(DocumentTermMatrix(corpus)))
}

# Iterative implementation of reduce from https://stat.ethz.ch/pipermail/r-help/2004-November/060224.html
#
# Fixme: calling with length(x) == 0 fails.
reduceZoneSpec<-function(f,b,x){
  n<-length(x)

  label = paste0(c(names(b), names(x)), collapse="&")
  rval<-f(unlist(x[n]),unlist(b))
  if (n>1){
    for (xn in rev(x[-1]))
      rval<-f(unlist(xn),unlist(rval))
  }

  r <- list(rval)
  names(r) <- c(label)
  return(r)
}

calculateZoneCardinality <- function(inz, outz) {
  l_inz <- inz[1]
  l_outz <- outz[1]
  
  if(length(inz) >= 2) {
    l_inz <- reduceZoneSpec(intersect, inz[1], inz[-1])
  }
  
  if(length(outz) >= 2) {
    l_outz <- reduceZoneSpec(union, outz[1], outz[-1])
  }
  

  #zone_desc <- paste(names(l_inz), names(l_outz), sep="-", collapse="")
  e <- c(length(setdiff(unlist(l_inz), unlist(l_outz))))
  names(e) <- names(l_inz)
  e
}

generateVennCombinations <- function(d) {
  labels <- sort(labels(d))
  unlist(foreach (i=1:length(labels)) %do% apply(combn(labels, i), 2, function (col) { 
      outs <- setdiff(labels, col)
 
      inz <- d[col]
      card <- calculateZoneCardinality(d[col], d[outs])
      list(card)
    }))
}

bookData <- function () {
  ulysses <- fileToWordList(file=u_book)
  bible <- fileToWordList(file=b_book)
  dorian_gray <- fileToWordList(file=d_book)
  silas_marner <- fileToWordList(s_book)
  moby_dick <- fileToWordList(m_book)
  macbeth <- fileToWordList(a_book)

  u <- list("U"=ulysses)
  b <- list("B"=bible)
  d <- list("D"=dorian_gray)
  s <- list("S"=silas_marner)
  m <- list("M"=moby_dick)
  a <- list("A"=macbeth)

  vennz <- c(u, b, d, s, m, a)
  generateVennCombinations(vennz)
}

pseudoRandomCombination <- function (cnum) {
  print(paste0("Getting ", cnum, " books"))
  bs <- sample(books, cnum)
  print(bs)
  vennz <- c()

  # horribly procedural
  i <- 1
  for(b in bs) {
    print(paste0("\tGetting book ", b, ".txt"))
    words = list(b=fileToWordList(file=paste0(b, ".txt")))
    names(words) <- letters[i]
    vennz = c(vennz, words)
    i <- i + 1
  }
  print(labels(vennz))
  generateVennCombinations(vennz)
}

downloadBooks(books)

checkCache("http://www.gutenberg.org/cache/epub/4300/pg4300.txt", u_book)
checkCache("http://www.gutenberg.org/cache/epub/10/pg10.txt", b_book)
checkCache("http://www.gutenberg.org/cache/epub/174/pg174.txt", d_book)
checkCache("http://www.gutenberg.org/cache/epub/550/pg550.txt", s_book)
checkCache("http://www.gutenberg.org/cache/epub/2701/pg2701.txt", m_book)
checkCache("http://www.gutenberg.org/cache/epub/2264/pg2264.txt", a_book)