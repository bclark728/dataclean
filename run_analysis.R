run_analysis <- function() {
  library(data.table)
  
  # get descriptive activity labels
  activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
  
  # get variable names
  variables <- read.table("./UCI HAR Dataset/features.txt")[,2]
  
  # read x&y variables from test subset
  testX <- read.table("./UCI HAR Dataset/test/X_test.txt")
  testY <- read.table("./UCI HAR Dataset/test/y_test.txt")
  
  # bind into common dataframe and translate numeric to descriptive labels
  test  <- cbind(activitylabels[unlist(testY)], testX)
  setnames(test, c("Activity", as.vector(variables)))

  # same steps for training subset
  trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")
  trainY <- read.table("./UCI HAR Dataset/train/y_train.txt")
  train  <- cbind(activitylabels[unlist(trainY)], trainX)
  setnames(train, c("Activity", as.vector(variables)))
  
  # build a data table by merging test and train subsets
  table  <- rbind(test, train)
  
  #clean up
  rm(testX, testY, test, trainX, trainY, train)
  
  # extract target variables
  target <- table[, c(grep("mean()", colnames(table)), grep("std()", colnames(table)))]
  
  #translate column names
  neatnames = lapply(colnames(target), decode)
  setnames(target, unlist(neatnames))
  
  target
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
    print(label)
    print(quant)
  
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
  
  print(ret)
  print("===============================")
  return(ret)
}