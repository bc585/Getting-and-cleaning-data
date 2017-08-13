#Pre-work- downloading data
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="./data/Dataset.zip")
  
  # Unzip dataSet to /data directory
  unzip(zipfile="./data/Dataset.zip",exdir="./data")


#1. Merges the training and the test sets to create one data set.
  # Reading trainings tables:
  x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  
  # Reading testing tables:
  x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  
  # Reading feature vector:
  features <- read.table('./data/UCI HAR Dataset/features.txt')
  
  # Reading activity labels:
  activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
  #Reading column names:
    
    colNames <- colnames(setAllInOne)
  # Create vector for defining ID, mean and standard deviation:
    
    mean_and_std <- (grepl("activityId" , colNames) | 
                       grepl("subjectId" , colNames) | 
                       grepl("mean.." , colNames) | 
                       grepl("std.." , colNames) 
    )
  #Making nessesary subset from setAllInOne:
    
    setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
  
  
#3.Uses descriptive activity names to name the activities in the data set
    setWithActivityNames <- merge(setForMeanAndStd, activityLabels,
                                  by='activityId',
                                  all.x=TRUE)
#4.Appropriately labels the data set with descriptive variable names.
    
    # Cleaning up the variable names
    for (i in 1:length(colNames)) 
    {
      colNames[i] = gsub("\\()","",colNames[i])
      colNames[i] = gsub("-std$","StdDev",colNames[i])
      colNames[i] = gsub("-mean","Mean",colNames[i])
      colNames[i] = gsub("^(t)","time",colNames[i])
      colNames[i] = gsub("^(f)","freq",colNames[i])
      colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
      colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
      colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
      colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
      colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
      colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
      colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
    };
    
    # Reassigning the new descriptive column names to the finalData set
    colnames(finalData) = colNames;
#5. From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject.
    
   #Making second tidy data set
    
    TidySet_2 <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
    TidySet_2 <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]
    
    #Writing second tidy data set in txt file
    
    write.table(TidySet_2, "TidySet_2.txt", row.name=FALSE)