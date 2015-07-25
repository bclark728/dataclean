run_analysis <- function(tidymain = "./main.txt", tidyaverages = "./averages.txt") {
  library(data.table)
  library(reshape2)
  
  # get variable names
  variables <- read.table("./UCI HAR Dataset/features.txt")[,2]
  
  # read x&y variables and activity&subject labels from test subset
  testX <- read.table("./UCI HAR Dataset/test/X_test.txt")
  testY <- read.table("./UCI HAR Dataset/test/y_test.txt")
  testS <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  # bind into common dataframe 
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
  
  neatactivities <- lapply(unlist(activities), translate)
  
  #clean up
  rm(testX, testY, trainX, trainY)
  
  # extract target variables
  target <- table[, c(1, grep("mean()", colnames(table)), grep("std()", colnames(table)))]
  
  # translate column names
  neatnames <- lapply(colnames(target), decode)
  
  # attach activity and subject labels
  target <- cbind(subjects, unlist(neatactivities), target)
  setnames(target, c("Subject", "Activity", unlist(neatnames)))
  
  # save main tidy dataset to disk
  write.table(target, tidymain)
  
  # build averages dataset and save
  melted = melt(target, id.vars=c("Subject", "Activity"))
  cast = dcast(melted, Subject + Activity ~ variable, mean)
  write.table(cast, tidyaverages)
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
    domain <- "Time"
  }
  else
  if(substr(label, 1, 1) == "f") {
    domain <- "Frequency"
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
  if(grepl("Mag", label)) {
    quantity <- append(quantity, "Magnitude")
  }
  # join into string
  quant <- paste(quantity, collapse = ".")
  
  # detect dimension tag, if present
  dimension <- ""
  if(grepl("-X", label)) {
    dimension <- "X"
  }
  else
  if(grepl("-Y", label)) {
    dimension <- "Y"
  }
  else
  if(grepl("-Z", label)) {
    dimension <- "Z"
  }
  
  ret <- paste(component, quant, measure, paste0(dimension, "Axis"), 
               paste0(domain, "Domain"), sep = ".")
  
  return(ret)
}

translate <- function(label) {
  # get descriptive activity labels
  activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
  
  return(activitylabels[label])
}