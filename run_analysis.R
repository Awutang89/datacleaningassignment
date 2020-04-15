run_analysis <- function(filepath) {
  
  #put the data into the correct data folder and then unzip the data to the main 
  url <- filepath
  if (!file.exists("./data")) {dir.create("./data")}
  download.file(url, destfile = "./data/accellerometers.zip")
  unzip("./data/accellerometers.zip")

  #take the data in zip and put them into data frames.
  features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
  activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
  
  #merge the data, test and train have the exact same number of the columns/variables I will combine them first
  x_total <- rbind(x_test, x_train)
  y_total <- rbind(y_test, y_train)
  subject_total <- rbind(subject_test, subject_train)

  #extract the mean and the standard deviation of a dataset
  library(dplyr)
  x_mean <- select(x_total,grep("mean",names(x_total)))
  x_std <- select(x_total,grep("std",names(x_total)))
  x_meanstd <- cbind(x_mean,x_std)
  
  #name the activities in the data set according to the activities data
  tempcode <- merge(y_total,activities, "code")
  #alternative way to do this as well using dyplyr commands-> mutate(y_total, activity = activities[code,2])
  activitymeanstd <- dplyr::bind_cols(x_meanstd,"activity" = tempcode[,2])  #binds activity to the mean and std data set
  activitymeanstd <- cbind(activitymeanstd, "subject" = subject_total[,1])
  
  #clean the variable names of the activity mean std data frame to make it easier to understand - # I left easy to remember abbreviations inside
  finalnames <- gsub("^t","time", names(activitymeanstd))  #change the naming from t to time
  finalnames <- gsub("^f","freq", finalnames) #change the naming for f to freq (frequency)
  finalnames <- gsub("\\.","_", finalnames)  #remove the . because of bad naming convention
  finalnames <- gsub("___","_", finalnames) #remove the ___ naming convention
  finalnames <- gsub("__","", finalnames)   #remove the open ended __ characters taht don't specify dimension
  finalnames <- gsub("bodybody","body",finalnames)     #remove the duplicate bodys
  
  #make the organized dataframe
  tidydata <- activitymeanstd
  names(tidydata) <- finalnames
  
  print(str(tidydata))
  
  #get data required as an average by activity and by subject
  finaldata <- group_by(tidydata, subject, activity) %>% summarise_all(funs(mean))
  print(str(finaldata))
  }

