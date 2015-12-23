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
  corpus <- VCorpus(DirSource(directory="/home/aidan/Projects/euleRTestHarness", pattern=file))
  
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
  ulysses <- fileToWordList(file="ulysses.txt")
  bible <- fileToWordList(file="bible.txt")
  dorian_gray <- fileToWordList(file="DorianGray.txt")
  silas_marner <- fileToWordList("SilasMarner.txt")
  moby_dick <- fileToWordList("MobyDick.txt")
  macbeth <- fileToWordList("Macbeth.txt")

  u <- list("U"=ulysses)
  b <- list("B"=bible)
  d <- list("D"=dorian_gray)
  s <- list("S"=silas_marner)
  m <- list("M"=moby_dick)
  a <- list("A"=macbeth)

  vennz <- c(u, b, d, s, m, a)
  generateVennCombinations(vennz)
}
