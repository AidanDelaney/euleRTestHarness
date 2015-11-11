#!Important: all tests can only be achieved with single letter labels.
#            It might also be a good idea to avoid the label 'X'

#d1 <- euleR(c(A=100, B=100, C=110, "A&B"=20, "A&C"=20,"A&B&C"=10))
#d2 <- euleR(c(A=200, B=100, C=110, "A&B"=20, "A&C"=20,"A&B&C"=10))
#d3 <- euleR(c(A=100, B=200, C=110, "A&B"=20, "A&C"=20,"A&B&C"=10))
#d4 <- euleR(c(A=100, B=100, C=310, "A&B"=20, "A&C"=20,"A&B&C"=10))

#my_data <- lapply(list(d1, d2, d3, d4), gather)

#all <- do.call("rbind", my_data)

dig <- function(x) x[1]

gather <- function (x) {
  df <- data.frame(t(sapply(x$areas, dig)))
  #df <- cbind(df, duration=x$duration)
}

runLevel <- function (id, area_spec) {
  vennom_level <- runVennomLevel (id, area_spec)
  venneuler_level <- runVennEulerLevel (id, area_spec)
}

runVennEulerLevel <- function (id, area_spec) {
  venneuler  <- venneuler(area_spec)
  
  # Now post some of the venneuler data away to compute the actual areas.
  circles <- cbind(venneuler$centers, diameter=venneuler$diameters)
  cs <- foreach (i=1:nrow(circles)) %do% c(circles[i,][1], circles[i,][2], circles[i,][3], label=rownames(circles)[i])
  json <- toJSON(list(circles=cs))
  
  # POST to web service
  httpheader <- c(Accept="application/json; charset=UTF-8",
                  "Content-Type"="application/json")
  result <- postForm('http://localhost:8080/areas', .opts=list(httpheader=httpheader
                                            , postfields=json))
  
  vf <- gather(result)
  #vf <- cbind(vf, treatment="venneuler")
  
  vf <- renameFrame(vf)
  print(vf)
}

runVennomLevel <- function (id, area_spec) {
  euler <- euleR(area_spec)

  # Euler Frame
  ef <- gather(euler)
  
  ef <- normalizeFrameLabels(ef)
  area_spec <- normalizeFrameLabels(area_spec)
  
  df = data.frame(t(c("duration"=euler$duration, "treatment"="vennom")))
  
  cmp <- populateFrame(id, area_spec, ef)
  print(df)
  print(cmp)
  #merge(cmp, df)
}

populateFrame <- function (diagram_id, expected_frame,  actual_frame) {
  # Assumption: underlying library calls (vennom/vennEuler) won't add in extra circle labels.
  required_circles <- unique(unlist(mapply(function(x) {strsplit(x, '')}, names(expected_frame))))
  required_zones <- names(expected_frame)
  actual_zones <- names(actual_frame)
  
  # Generate Venn-set of expected_frame, setting any unset rows to 0.0
  venn_set <- generateVennSet(required_circles)

  # At this stage the names of frames are subsets of venn_set
  # Get subset of actual_frame that is in expected_frame
  diff    <- setdiff(venn_set, names(actual_frame))
  
  # Set other areas of the venn set to 
  actual_frame[unlist(diff)] <- 0.0
  
  # Normalise each frame such that all areas add up to 1.0
  actual_frame   <- normalizeFrame(actual_frame)
  
  # diagram id, Pearson coefficient, number of circles, number of zones required, # over requirements, # under requirements
  #length(circles) is number of circles
  data.frame(t(c(id=diagram_id, num_required_zone=length(required_zones), num_required_circles=length(required_circles), venn_data=actual_frame)))
}

dropColumns <- function(frame, columns) {
  droppage <- names(frame) %in% columns
  frame[!droppage]
}

generateVennSet <- function(labels) {
  labels <- sort(labels)
  unlist(foreach (i=1:length(labels)) %do% apply(combn(labels, i), 2, function (col) { paste(unique(col), sep="", collapse = "")}))
}

normalizeFrame <- function (venn_data) {
  sum <- sum(venn_data)
  venn_data <- mapply(function(x) {x/sum}, venn_data)
}

normalizeFrameLabels <- function (df) {
  # Rename things in the Euler Frame for easier reading (and merging with the venneuler frame)
  names(df) <- gsub("\\.\\.\\.\\.", "-", names(df))
  names(df) <- gsub("^X\\.\\.", "", names(df))
  names(df) <- gsub("\\.\\.", "", names(df))
  # All names are now in the form "inset-outset" where each set is of the form "P.Q,R"
  names(df) <- gsub("&", "", names(df))
  
  # Now remove everything in the outset i.e. from the '-' onwards
  # This allows us to compare the output results with the input specification
  names(df) <- gsub("-.*$", "", names(df))
  names(df) <- lapply(names(df), strSort)
  df
}

strSort <- function(x) {
  sapply(lapply(strsplit(x, NULL), sort), paste, collapse="")
}