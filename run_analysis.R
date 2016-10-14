# section 0
library(dplyr)
c0 <- read.table("./UCI HAR Dataset/activity_labels.txt",stringsAsFactors=FALSE )
c1 <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors=FALSE )

# section 1 
d1 <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = c1[,2])
d22 <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(d22)[1]<-"subject"
d2 <- select(d22, subject)
d3 <- read.table("./UCI HAR Dataset/train/Y_train.txt")
names(d3)[1]<-"activity"
d4 <- c0[d3[,1],2]


# section 2
c2 <- select(d1, contains("mean.."), contains("std.."))
fin1 <- tbl_df(cbind(d2,d4,c2))
names(fin1)[2] <- "activity"
rm("d1")
rm("d2")
rm("d22")
rm("d3")
rm("d4")


# section 3
e1 <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = c1[,2])
e22 <- read.table("./UCI HAR Dataset/test/subject_test.txt")
names(e22)[1]<-"subject"
e2 <- select(e22, subject)
e3 <- read.table("./UCI HAR Dataset/test/Y_test.txt")
names(e3)[1]<-"activity"
e4 <- c0[e3[,1],2]


# section 4
f2 <- select(e1, contains("mean.."), contains("std.."))
fin2 <- tbl_df(cbind(e2,e4,f2))
names(fin2)[2] <- "activity"
rm("e1")
rm("e2")
rm("e22")
rm("e3")
rm("e4")


# section 5
final <- rbind(fin1, fin2)
final1 <- arrange(final, subject) %>% group_by( subject, activity)
final1 <-  final1 %>% setNames(gsub("^f", "Frequency_", names(.)))
final1 <-   final1 %>%  setNames(gsub("^t", "Time_", names(.)))
final1 <-  final1 %>%   setNames(gsub("Acc", "Accelerometer_", names(.)))
final1 <-  final1 %>%   setNames(gsub("Gyro", "Gyroscope_", names(.)))
final1 <-  final1 %>%   setNames(gsub("Mag", "Magnitude_", names(.)))
final1 <- final1 %>% setNames(gsub("mean\\.\\.", "Mean", names(.)))
final1 <- final1 %>% setNames(gsub("std\\.\\.", "Std", names(.)))
final1 <-   final1 %>%  setNames(gsub("\\.", "", names(.)))



# section 6
out <- summarise_each (final1, funs(mean))
write.table(out, file = "out.txt", sep = ",", 
            col.names = NA, qmethod = "double")