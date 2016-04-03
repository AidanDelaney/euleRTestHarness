# test.R
# A test script to run comparative tests between euleR and venneuler R libs.
#
# The core task is to generate area-proportional Euler diagrams using (a) euleR
# and (b) venneuler.  To do this we:
#
#  1. define an area specification.
#  2. generate both an euler and a venneuler diagram.
#  3. compare the area specification with the actual results from each treatment
#
# The comparison is achieved by comparing the area specificaiton for a Venn
# diagram with the Venn diagram specification obtained from euleR/venneuler. The
# following is an example:
#
# Let `s1 <- c(A=100, B=100, "A&C"=50)` be an area specification.  This
# introduces the contours A, B, and C into our diagram.  Suppose the euleR
# library returns zone (A, BC) with area 98, (AC, B) with area 70 and (B, AC)
# with area 100.
#
# The original area specification is expanded to (A, BC) = 100, (B, AC) = 100,
# (AC, B) = 50, (AB, C) = 0, (ABC, ) = 0, (BC, A) = 0, (C, AB) = 0.  The
# returned specification from euleR is similarly expanded and all zones that are
# not present are given area 0.
#
# From the "Venn-set" of zones in the area specification and that returned by
# euleR, we then use the `cor` function in R to compute the Pearson correlation
# coefficient.  A similar correlation is made between the area specifcation and
# the results returned from venneuler.

#!Important: all tests can only be achieved with single letter labels.
#            It might also be a good idea to avoid the label 'X'

#!Important: all tests require a locally running instance of [Köttelbrücke](https://github.com/AidanDelaney/K-ttelbr-cke)

library("euleR")
library("venneuler")

source("src/books.R")
# specs
# Adding a spec:
#
#  * You'll need to add your area specification below.
#  * Thereafter, you'll have to extend the list of calls to `runLevel` to
#    include your new area spec.
#
# TODO: automate all of this.
s1 <- c(A=100, B=100, C=100, "A&B"=20, "A&C"=20, "B&C"=20, "A&B&C"=10) # Venn 3
s2 <- c(A=200, "A&B"=100) # Tunnel 2
s3 <- c(A = 400, B = 600,C = 300, D = 200, E = 700, F = 300, "A&B" = 200, "A&F" = 200, "B&C" = 200, "B&D" = 100, "B&F" = 200, "C&D" = 100, "D&E" = 100, "E&F" = 100, "A&B&F" = 100, "B&C&D" = 100)
# SE ->  S, Treat -> T, Anti-CCP -> A ,DAS28 -> D
s4 <- c(S = 130, T = 280, A = 1010, D = 910, "S&T" = 10, "D&S" = 140, "T&A" = 60, "A&D&S" = 10)
s5 <- bookData()

t1 <- c(A=100, B=100, C=100, D=100, "A&B"=50, "A&C"=50, "A&D"=50, "B&C"=50, "B&D"=50, "C&D"=50, "A&B&C"=20, "A&C&D"=20, "B&C&D"=20, "A&B&C&D"=5)

# When we can automagically generate level id's, then this will run everything.
#spec_list <- list(s1, s2, s3, s4)
#all <- do.call("rbind", lapply(spec_list, runLevel())

# Used to grab some data from the child of a JSON object
dig <- function(x) x[1]

# Get the 'areas' information from a JSON object.
gather <- function (x) {
  df <- data.frame(t(sapply(x$areas, dig)))
}

# A level is both treatments of an input area spec.
#
# TODO: generate the id from the name of the area_spec, this allows the
# automation referred to above.
runLevel <- function (id, area_spec) {

  # FIXME: Evil scale hack.
  area_spec <- sapply(area_spec, function(x) { x * 100 })
  #print(area_spec)

  vennom_level <- runVennomLevel (id, area_spec)
  venneuler_level <- runVennEulerLevel (id, area_spec)

  res <- rbind(vennom_level, venneuler_level)
  report(res)
  res
}

getAreas <- function (circles) {
 json <- toJSON(list(circles=circles))

  # POST to web service
  httpheader <- c(Accept="application/json; charset=UTF-8",
                  "Content-Type"="application/json")
  # hardcoded URL as testharness is only ever run by someone who has a local running service.
  result <- postForm('http://localhost:8080/areas', .opts=list(httpheader=httpheader
                                            , postfields=json))
}

# Run VennEuler for a single input area_spec
runVennEulerLevel <- function (id, area_spec) {
  # Run venneuler and time how long it takes in milliseconds.
  duration <- system.time(venneuler  <- venneuler(area_spec))

  # Now post some of the venneuler data away to compute the actual areas.
  radii = mapply(function(d) {d/2.0}, venneuler$diameters) # convert diameters into radii
  circles <- cbind(venneuler$centers, radius=radii)
  cs <- foreach (i=1:nrow(circles)) %do% c(circles[i,][1], circles[i,][2], circles[i,][3], label=rownames(circles)[i])

  result <- getAreas(cs)

  # plot the diagram and save it
  svg(paste(id, "venneuler.svg", sep="-"))
  plot(venneuler)
  dev.off()

  # assemble the results into a dataframe that we can analyse.
  vf <- gather(fromJSON(result))

  # make all the labels in both data_frames syntactically the same
  vf <- normalizeFrameLabels(vf)
  area_spec <-normalizeFrameLabels(area_spec)

  # get the duration in milliseconds
  df  = data.frame(t(c("duration"=as.integer(duration["elapsed"] * 1000), "treatment"="venneuler")))

  # do the actual comparision of area_spec and vf
  cmp <- populateFrame(id, area_spec, vf)

  cbind(df, cmp)
}

# Run the vennom treatment with an area specification
runVennomLevel <- function (id, area_spec) {
  duration <- system.time(euler <- euleR(area_spec))

  # plot the diagram and save it
  svg(paste(id,"vennom.svg", sep="-"))
  plot(euler)
  dev.off()

  t_areas <- euler$areas
  #print(euler$circles)
  #print(t_areas)

  result <- getAreas(euler$circles)

  # Euler Frame
  ef <- gather(fromJSON(result))
  #print(ef)

  ef <- normalizeFrameLabels(ef)
  area_spec <- normalizeFrameLabels(area_spec)

  # In this case, the webservice passes back the duration of how long the layout
  # algorithm took to compute the layout.
  df = data.frame(t(c("duration"=euler$duration, "treatment"="vennom")))

  cmp <- populateFrame(id, area_spec, ef)

  cbind(df, cmp)
}

frameToString <- function(df) {
  r_string <- ""
  for(i in names(df)) {
        r_string <- paste(r_string, i, "=", df[[i]], sep=" ")
  }
  r_string
}

# Peform the correlation computation
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
  diffe   <- setdiff(venn_set, names(expected_frame))
  missing <- setdiff(names(expected_frame), names(actual_frame))
  extra   <- setdiff(names(actual_frame), names(expected_frame))

  # Set other areas of the venn set to
  actual_frame[unlist(diff)] <- 0.0
  expected_frame[unlist(diffe)] <- 0.0

  # Normalise each frame such that all areas add up to 1.0
  actual_frame   <- normalizeFrame(actual_frame)
  expected_frame <- normalizeFrame(expected_frame)
  
#  print(frameToString(expected_frame))
#  print(frameToString(actual_frame))

  # diagram id, Pearson coefficient, number of circles, number of zones required, # over requirements, # under requirements
  #length(circles) is number of circles
  pearson_coeff <- cor(actual_frame, expected_frame)
  data.frame(t(c(id=diagram_id, pearson_coeffecient=pearson_coeff, num_circles=length(required_circles), num_required_zones=length(required_zones), num_extra_zones=length(extra), num_missing_zones=length(missing), num_actual_zones=length(actual_zones), expected_spec=frameToString(expected_frame), actual_value=frameToString(actual_frame))))
}

# Given the labels of the contours, generate the venn set of zones (in a
# specific microsyntax)
generateVennSet <- function(labels) {
  labels <- sort(labels)
  unlist(foreach (i=1:length(labels)) %do% apply(combn(labels, i), 2, function (col) { paste(unique(col), sep="", collapse = "")}))
}

# Given a frame consting of only numeric values, normalise them in the range
# 0.0 to 1.0
normalizeFrame <- function (venn_data) {
  sum <- sum(venn_data)
  venn_data <- mapply(function(x) {x/sum}, venn_data)
}

# Given a label returned from either euleR or venneuler, turn it into some kind
# of normal form for labels.  Then apply this normalisation to the input data
# frame.
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

  # Order the dataframe by alphabetic names
  df[order(names(df))]
}

# Sort a string in lexiographic order.
strSort <- function(x) {
  sapply(lapply(strsplit(x, NULL), sort), paste, collapse="")
}

report <- function(x) {
  write.table(x[c("id", "treatment", "num_circles", "num_required_zones", "num_actual_zones", "num_extra_zones", "num_missing_zones", "pearson_coeffecient", "duration")], file="results.csv", row.names=FALSE, quote=FALSE, sep=",", append=TRUE, col.names = FALSE)
}

# This runs the actual experiment
df1 <- runLevel("s1", s1)
# Overwrite the results with the colnames -- pretty evil, but it works.
write.table(df1[c("id", "treatment", "num_circles", "num_required_zones", "num_actual_zones", "num_extra_zones", "num_missing_zones", "pearson_coeffecient", "duration")], file="results.csv", row.names=FALSE, quote=FALSE, sep=",", append=FALSE, col.names = TRUE)

df2 <- runLevel("s2", s2)
df3 <- runLevel("s3", s3)
df4 <- runLevel("s4", s4)
df5 <- runLevel("s5", s5)
df6 <- runLevel("t1", t1)

# This is a silly way of doing things, but it allows us to pinpoint failures more easily.
set.seed(0)
r1 <- pseudoRandomCombination(sample(2:10,1))

# Again, horribly procedural, but allows us to isolate instances of failure for further processing
#print(r1)
rf1 <- runLevel("1", r1)

# Won;t run due to issues in VennEuler package.
r2 <- pseudoRandomCombination(sample(2:10,1))
##print(r2)
rf2 <- runLevel("2", r2)

r3 <- pseudoRandomCombination(sample(2:10,1))
#print(r3)
rf3 <- runLevel("3", r3)

r4 <- pseudoRandomCombination(sample(2:10,1))
#print(r4)
rf4 <- runLevel("4", r4)
#report(rf4)

r5 <- pseudoRandomCombination(sample(2:10,1))
print(r5)
rf5 <- runLevel("5", r5)
#report(rf5)

r6 <- pseudoRandomCombination(sample(2:10,1))
#print(r6)
rf6 <- runLevel("6", r6)
#report(rf6)

r7 <- pseudoRandomCombination(sample(2:10,1))
#print(r7)
rf7 <- runLevel("7", r7)
#report(rf7)

r8 <- pseudoRandomCombination(sample(2:10,1))
print(r8)
rf8 <- runLevel("8", r8)
#report(rf8)

r9 <- pseudoRandomCombination(sample(2:10,1))
print(r9)
rf9 <- runLevel("9", r9)
#report(rf9)

r10 <- pseudoRandomCombination(sample(2:10,1))
print(r10)
rf10 <- runLevel("10", r10)
#write.table(rf10[c("id", "treatment", "num_circles", "num_required_zones", "num_actual_zones", "num_extra_zones", "num_missing_zones", "pearson_coeffecient", "duration")], file="results.csv", row.names=FALSE, quote=FALSE, append=TRUE, sep=",", col.names=FALSE)

r11 <- pseudoRandomCombination(sample(2:10,1))
rf11 <- runLevel("11", r11)
#report(rf11)

r12 <- pseudoRandomCombination(sample(2:10,1))
rf12 <- runLevel("12", r12)
#report(rf12)

r13 <- pseudoRandomCombination(sample(2:10,1))
rf13 <- runLevel("13", r13)
#report(rf13)


r14 <- pseudoRandomCombination(sample(2:10,1))
rf14 <- runLevel("14", r14)
#report(rf14)

r15 <- pseudoRandomCombination(sample(2:10,1))
rf15 <- runLevel("15", r15)
#report(rf15)

r16 <- pseudoRandomCombination(sample(2:10,1))
rf16 <- runLevel("16", r16)
#report(rf16)

r17 <- pseudoRandomCombination(sample(2:10,1))
rf17 <- runLevel("17", r17)
#report(rf17)



r18 <- pseudoRandomCombination(sample(2:10,1))
rf18 <- runLevel("18", r18)
#report(rf18)


r19 <- pseudoRandomCombination(sample(2:10,1))
rf19 <- runLevel("19", r19)
#report(rf19)


r20 <- pseudoRandomCombination(sample(2:10,1))
rf20 <- runLevel("20", r20)
#report(rf20)

r21 <- pseudoRandomCombination(sample(2:10,1))
rf21 <- runLevel("21", r21)
#report(rf21)


r22 <- pseudoRandomCombination(sample(2:10,1))
rf22 <- runLevel("22", r22)
#report(rf22)

r23 <- pseudoRandomCombination(sample(2:10,1))
rf23 <- runLevel("23", r23)
#report(rf23)

r24 <- pseudoRandomCombination(sample(2:10,1))
rf24 <- runLevel("24", r24)
#report(rf24)

r25 <- pseudoRandomCombination(sample(2:10,1))
rf25 <- runLevel("25", r25)
#write.table(rf25[c("id", "treatment", "num_circles", "num_required_zones", "num_actual_zones", "num_extra_zones", "num_missing_zones", "pearson_coeffecient", "duration")], file="results.csv", row.names=FALSE, quote=FALSE, append=TRUE, sep=",", col.names=FALSE)

r26 <- pseudoRandomCombination(sample(2:10,1))
rf26 <- runLevel("26", r26)
#report(rf26)

r27 <- pseudoRandomCombination(sample(2:10,1))
rf27 <- runLevel("27", r27)
#report(rf27)

r28 <- pseudoRandomCombination(sample(2:10,1))
rf28 <- runLevel("28", r28)
#report(rf28)

r29 <- pseudoRandomCombination(sample(2:10,1))
rf29 <- runLevel("29", r29)
#report(rf29)

r30 <- pseudoRandomCombination(sample(2:10,1))
rf30 <- runLevel("30", r30)
#report(rf30)

# combine results in a single dataframe
#all <- rbind(df1, df2, df3, df4, df5, df6, rf1, rf2, rf3, rf4, rf5, rf6, rf7, rf8, rf9, rf10)
#all <- rbind(rf1, rf3, rf4, rf5, rf7, rf8, rf9, rf10
#                  , rf12,  rf13, rf14, rf15, rf16, rf17, rf18, rf19, rf20
#                  , rf21, rf22,  rf23, rf24, rf25, rf26, rf27, rf28, rf29, rf30
#                  )
