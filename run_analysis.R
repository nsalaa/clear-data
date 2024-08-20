run_analysis <- function() {

     library(reshape2)


     traindata <- read.table("./train/X_train.txt")
     testdata  <- read.table("./test/X_test.txt")
     joindata  <- rbind(traindata, testdata) 




     trainlabel <- read.table("./train/y_train.txt")
     testlabel  <- read.table("./test/y_test.txt")
     joinlabel  <- rbind(trainlabel, testlabel)

     trainsubject <- read.table("./train/subject_train.txt")
     testsubject  <- read.table("./test/subject_test.txt")
     joinsubject  <- rbind(trainsubject, testsubject)



     features <- read.table("features.txt") 



     meanstdindex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])


     joindatanew <- joindata[, meanstdindex] 


     colnames(joindatanew) <- features[meanstdindex, 2] 


     colnames(joindatanew) <- gsub("\\(|\\)", "", colnames(joindatanew)) 
     colnames(joindatanew) <- gsub("-", ".", colnames(joindatanew))
     colnames(joindatanew) <- tolower(colnames(joindatanew))



     activity <- read.table("activity_labels.txt")


     activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

     activitylabel <- activity[joinlabel[, 1], 2]


     joinlabel[, 1] <- activitylabel 

    colnames(joinlabel) <- "activity"




     colnames(joinsubject) <- "subject"



     cleandata <- cbind(joinsubject, joinlabel, joindatanew)






     meltdfrm <- melt(cleandata, id=c("activity", "subject"))


     tidydfrm <- dcast(meltdfrm, activity + subject ~ variable, mean)


     write.table(tidydfrm, "tidy_data.txt", row.names = F, col.names= T, sep = "\t")

 
     


}

