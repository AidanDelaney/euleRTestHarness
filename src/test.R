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

# specs
# Adding a spec:
#
#  * You'll need to add your area specification below.
#  * Thereafter, you'll have to extend the list of calls to `runLevel` to
#    include your new area spec.
#
# TODO: automate all of this.
s1 <- c(A=100, B=100, C=110, "A&B"=20, "A&C"=20,"A&B&C"=10)
s2 <- c(A=200, B=100, C=110, "A&B"=20, "A&C"=20,"A&B&C"=10)
s3 <- c(A=100, B=200, C=110, "A&B"=20, "A&C"=20,"A&B&C"=10)
s4 <- c(A=100, B=100, C=310, "A&B"=20, "A&C"=20,"A&B&C"=10)

# When we can automagically generate level id's, then this will run everything.
#spec_list <- list(s1, s2, s3, s4)
#all <- do.call("rbind", lapply(spec_list, runLevel())
#write.csv2(rbind(all), file="results.csv")

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
  vennom_level <- runVennomLevel (id, area_spec)
  venneuler_level <- runVennEulerLevel (id, area_spec)

  rbind(vennom_level, venneuler_level)
}

# Run VennEuler for a single input area_spec
runVennEulerLevel <- function (id, area_spec) {
  # Run venneuler and time how long it takes in milliseconds.
  duration <- system.time(venneuler  <- venneuler(area_spec))

  # Now post some of the venneuler data away to compute the actual areas.
  radii = mapply(function(d) {d/2.0}, venneuler$diameters) # convert diameters into radii
  circles <- cbind(venneuler$centers, radius=radii)
  cs <- foreach (i=1:nrow(circles)) %do% c(circles[i,][1], circles[i,][2], circles[i,][3], label=rownames(circles)[i])
  json <- toJSON(list(circles=cs))

  # POST to web service
  httpheader <- c(Accept="application/json; charset=UTF-8",
                  "Content-Type"="application/json")
  # hardcoded URL as testharness is only ever run by someone who has a local running service.
  result <- postForm('http://localhost:8080/areas', .opts=list(httpheader=httpheader
                                            , postfields=json))

  # plot the diagram and save it
  plot(venneuler)
  pdf(paste(id, "venneuler.pdf", sep="-"))

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
  euler <- euleR(area_spec)

  # plot the diagram and save it
  plot(euler)
  pdf(paste(id, "vennom.pdf", sep="-"))

  # Euler Frame
  ef <- gather(euler)

  ef <- normalizeFrameLabels(ef)
  area_spec <- normalizeFrameLabels(area_spec)

  # In this case, the webservice passes back the duration of how long the layout
  # algorithm took to compute the layout.
  df = data.frame(t(c("duration"=euler$duration, "treatment"="vennom")))

  cmp <- populateFrame(id, area_spec, ef)

  cbind(df, cmp)
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

  # diagram id, Pearson coefficient, number of circles, number of zones required, # over requirements, # under requirements
  #length(circles) is number of circles
  pearson_coeff <- cor(actual_frame, expected_frame)
  data.frame(t(c(id=diagram_id, pearson_coeffecient=pearson_coeff, num_required_zones=length(required_zones), num_extra_zones=length(extra), num_missing_zones=length(missing), num_actual_zones=length(actual_zones))))
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
  df
}

# Sort a string in lexiographic order.
strSort <- function(x) {
  sapply(lapply(strsplit(x, NULL), sort), paste, collapse="")
}

# Turn off display of generated plots
graphics.off()

# This runs the actual experiment
df1 <- runLevel("s1", s1)
df2 <- runLevel("s2", s2)
df3 <- runLevel("s3", s3)
df4 <- runLevel("s4", s4)

# combine results in a single dataframe
all <- rbind(df1, df2, df3, df4)

# output everything to a CSV file.
write.csv(all[c("id", "treatment", "num_required_zones", "num_actual_zones", "num_extra_zones", "num_missing_zones", "pearson_coeffecient", "duration")], file="results.csv", row.names=FALSE, quote=FALSE)