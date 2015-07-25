##
# run_analysis.R
#
#  a script for cleaning and summarizing accelerometer and gyroscope data
##

# run_analysis()
#  optional arguments:
#   - tidymain: path to output a table.read()-readable data.table that contains all
#               the combined data from the test and train data subsets
#   - tidyaverages: path to output a table.read()-readable data.table that contains
#                   averaged values for all variables, sorted by test subject and 
#                   activity
#  returns:
#   - a data.table containing the average data

run_analysis <- function(tidymain = "./main.txt", tidyaverages = "./averages.txt") {
  library(data.table)
  library(reshape2)
  
  # get variable names
  variables <- read.table("./UCI HAR Dataset/features.txt")[,2]
  
  # read x&y variables and subject labels from test subset
  testX <- read.table("./UCI HAR Dataset/test/X_test.txt")
  testY <- read.table("./UCI HAR Dataset/test/y_test.txt")
  testS <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  setnames(testX, as.vector(variables))

  # same steps for training subset
  trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")
  trainY <- read.table("./UCI HAR Dataset/train/y_train.txt")
  trainS <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  setnames(trainX, as.vector(variables))
  
  # build a data table by merging test and train subsets
  table      <- rbind(testX, trainX)
  activities <- rbind(testY, trainY)
  subjects   <- rbind(testS, trainS)
  
  # translate numeric activity labels into human-readable
  neatactivities <- lapply(unlist(activities), translate)
  
  # extract target variables
  target <- table[, c(1, grep("mean()", colnames(table)), grep("std()", 
                                                               colnames(table)))]
  
  # translate column names
  neatnames <- lapply(colnames(target), decode)
  
  # attach activity and subject labels
  target <- cbind(subjects, unlist(neatactivities), target)
  setnames(target, c("Subject", "Activity", unlist(neatnames)))
  
  # save main tidy dataset to disk
  write.table(target, tidymain, row.names = FALSE)
  
  # build averages dataset and save
  melted = melt(target, id.vars=c("Subject", "Activity"))
  cast = dcast(melted, Subject + Activity ~ variable, mean)
  write.table(cast, tidyaverages, row.names = FALSE)
  
  # return data.table
  return(data.table(cast))
}

## This helper function makes variables readable
decode <- function(label) {
  # detect whether mean or standard deviation
  measure <- ""
  if(grepl("mean", label)) {
    measure <- "Mean"
  }
  else
  if(grepl("std", label)) {
    measure <- "StandardDeviation"
  }
  else {
    stop(paste("ERROR! No valid measure in: ", label))
  }
  
  # detect domain
  domain <- ""
  if(substr(label, 1, 1) == "t") {
    domain <- "TimeDomain"
  }
  else
  if(substr(label, 1, 1) == "f") {
    domain <- "FrequencyDomain"
  }
  else {
    stop(paste("ERROR! No valid domain in: ", label))
  }
  
  # detect whether body or gravity
  component <- ""
  if(grepl("Body", label)) {
    component <- "Body"
  }
  else
  if(grepl("Gravity", label)) {
    component <- "Gravity"
  }
  else {
    stop(paste("ERROR! No valid component in: ", label))
  }
  
  # detect quantity measured, build serially
  quantity <- list()
  if(grepl("Acc", label)) {
    quantity[1] <- "Acceleration"
  }
  if(grepl("Gyro", label)) {
    quantity[1] <- "Gyroscopic"
  }
  if(grepl("Jerk", label)) {
    quantity <- append(quantity, "Jerk")
  }
  # join into string
  quant <- paste(quantity, collapse = ".")
  
  # detect dimension tag, if present
  dimension <- ""
  if(grepl("-X", label)) {
    dimension <- "XAxis"
  }
  else
  if(grepl("-Y", label)) {
    dimension <- "YAxis"
  }
  else
  if(grepl("-Z", label)) {
    dimension <- "ZAxis"
  }
  else
  if(grepl("Mag", label)) {
    dimension <- "Magnitude"
  }
  else {
    stop(paste0("ERROR! No valid dimension in: ", label))
  }
  
  ret <- paste(component, quant, measure, dimension, domain, sep = ".")
  
  return(ret)
}

## this is a helper function to make activity names human readable
translate <- function(label) {
  # get descriptive activity labels
  activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
  
  return(activitylabels[label])
}