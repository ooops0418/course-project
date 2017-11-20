# read the base set

subject_test <- read.table("subject_test.txt")
subject_train <- read.table("subject_train.txt")
activity_test <- read.table("y_test.txt")
activity_train <- read.table("y_train.txt")
data_test <- read.table("X_test.txt")
data_train <- read.table("X_train.txt")

# Merge two files
readAdditionalFile <- function(dataDirectory, filePath) {
        filePathTest <- paste(dataDirectory, "/test/", filePath, "_test.txt", sep="")
        filePathTrain <- paste(dataDirectory, "/train/", filePath, "_train.txt", sep="")
        data <- c(read.table(filePathTest)[,"V1"], read.table(filePathTrain)[,"V1"])
        data
}


# Read sets and returns a complete sets
readSets <- function(dataDirectory) {
  # Adding main data files (X_train and X_test)
  featuresFilePath <- paste(dataDirectory, "/features.txt", sep="")
  features <- read.table(featuresFilePath)[,"V2"]
  filteredFeatures <- sort(union(grep("mean\\(\\)", features), grep("std\\(\\)", features)))
  features <- correctFeatureName(features)
  set <- readBaseSet(paste(dataDirectory, "/test/X_test.txt", sep=""), filteredFeatures, features)
  set <- rbind(set, readBaseSet(paste(dataDirectory, "/train/X_train.txt", sep=""), filteredFeatures, features))
  
  # Adding subjects
  set$subject <- readAdditionalFile("UCI HAR Dataset", "subject")
  
  # Adding activities
  activitiesFilePath <- paste(dataDirectory, "/activity_labels.txt", sep="")
  activities <- read.table(activitiesFilePath)[,"V2"]
  set$activity <- activities[readAdditionalFile("UCI HAR Dataset", "y")]
  
  set
}

# From sets, creates summarized dataset 
createSummaryDataset <- function(dataDirectory) {
        sets <- readSets(dataDirectory)
        sets_x <- sets[,seq(1, length(names(sets)) - 2)]
        summary_by <- by(sets_x,paste(sets$subject, sets$activity, sep="_"), FUN=colMeans)
        summary <- do.call(rbind, summary_by)
        summary
}

dataDirectory <- "UCI HAR Dataset"
if (!file.exists(dataDirectory)) {
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  tmp_file <- "./temp.zip"
  download.file(url,tmp_file, method="curl")
  unzip(tmp_file, exdir="./")
  unlink(tmp_file)
}

summary <- createSummaryDataset(dataDirectory)
write.table(summary, "tidy.txt")
