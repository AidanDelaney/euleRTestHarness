generateVennCombin <- function(d) {
  unlist(foreach (i=1:length(labels(d))) %do% apply(combn(labels(d), i), 2, function (col) { 
    label = paste0(col, collapse="&")
    label
  }))
}

randCircles <- function () {
      # can't have 10 contours, as the contour names '10' need to be single letters.
	    num_contours <- sample(2:9, 1)
	    vennz <- (1:num_contours)
	    area_spec <- generateVennCombin(vennz)
	    dist <- sample(0:100, 2^num_contours - 1, replace=TRUE)
	    names(dist) <- area_spec
	    dist
}