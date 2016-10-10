
# section 0
library(dplyr)
library(tidyr)

c0 <- read.table("./UCI HAR Dataset/activity_labels.txt",stringsAsFactors=FALSE )
c1 <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors=FALSE )


# section 1 
d1 <- read.table("./UCI HAR Dataset/train/X_train.txt")
d2 <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(d2)[1]<-"subject"
d3 <- read.table("./UCI HAR Dataset/train/Y_train.txt")
names(d3)[1]<-"activity"
d4 <- c0[d3[,1],2]


# section 2
c2 <- select(d1,   V1: V6)
names(c2)[1:6]<-c1[1:6,2]
rm("d1")
fin1 <- cbind(d2,d4,c2)
names(fin1)[2] <- "activity"


# section 3
e1 <- read.table("./UCI HAR Dataset/test/X_test.txt")
e2 <- read.table("./UCI HAR Dataset/test/subject_test.txt")
names(e2)[1]<-"subject"
e3 <- read.table("./UCI HAR Dataset/test/Y_test.txt")
names(e3)[1]<-"activity"
e4 <- c0[e3[,1],2]


# section 4
f2 <- select(e1,   V1: V6)
names(f2)[1:6]<-c1[1:6,2]
rm("e1")
fin2 <- cbind(e2,e4,f2)
names(fin2)[2] <- "activity"


# section 5
final <- full_join(fin1, fin2, by =  
                           c("subject" , "activity" )  )
final1 <- gather(final, key = "variable" , value = "value", -c(1:2)
)

# section 6
# subject
final2 <- filter(final1, variable == c("tBodyAcc-mean()-X.x" , 
                                       "tBodyAcc-mean()-X.y" ) )
final2 <- group_by(final2, subject )
out <- summarise(final2,  "tBodyAcc-mean()-X" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-mean()-X-subject.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final3 <- filter(final1, variable == c("tBodyAcc-mean()-Y.x" , 
                                       "tBodyAcc-mean()-Y.y" ) )
final3 <- group_by(final3, subject )
out <- summarise(final3,  "tBodyAcc-mean()-Y" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-mean()-Y-subject.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final4 <- filter(final1, variable == c("tBodyAcc-mean()-Z.x" , 
                                       "tBodyAcc-mean()-Z.y" ) )
final4 <- group_by(final4, subject )
out <- summarise(final4,  "tBodyAcc-mean()-Z" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-mean()-Z-subject.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final5 <- filter(final1, variable == c("tBodyAcc-std()-X.x" , 
                                       "tBodyAcc-std()-X.y" ) )
final5 <- group_by(final5, subject )
out <- summarise(final5,  "tBodyAcc-std()-X" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-std()-X-subject.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final6 <- filter(final1, variable == c("tBodyAcc-std()-Y.x" , 
                                       "tBodyAcc-std()-Y.y" ) )
final6 <- group_by(final6, subject )
out <- summarise(final6,  "tBodyAcc-std()-Y" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-std()-Y-subject.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final7 <- filter(final1, variable == c("tBodyAcc-std()-Z.x" , 
                                       "tBodyAcc-std()-Z.y" ) )
final7 <- group_by(final7, subject )
out <- summarise(final7,  "tBodyAcc-std()-Z" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-std()-Z-subject.txt", sep = ",", 
            col.names = NA, qmethod = "double")

# section 7
# activity
final22 <- filter(final1, variable == c("tBodyAcc-mean()-X.x" , 
                                        "tBodyAcc-mean()-X.y" ) )
final22 <- group_by(final22, activity )
out <- summarise(final22,  "tBodyAcc-mean()-X" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-mean()-X-activity.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final33 <- filter(final1, variable == c("tBodyAcc-mean()-Y.x" , 
                                        "tBodyAcc-mean()-Y.y" ) )
final33 <- group_by(final33, activity )
out <- summarise(final33,  "tBodyAcc-mean()-Y" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-mean()-Y-activity.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final44 <- filter(final1, variable == c("tBodyAcc-mean()-Z.x" , 
                                        "tBodyAcc-mean()-Z.y" ) )
final44 <- group_by(final44, activity )
out <- summarise(final44,  "tBodyAcc-mean()-Z" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-mean()-Z-activity.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final55 <- filter(final1, variable == c("tBodyAcc-std()-X.x" , 
                                        "tBodyAcc-std()-X.y" ) )
final55 <- group_by(final55, activity )
out <- summarise(final55,  "tBodyAcc-std()-X" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-std()-X-activity.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final66 <- filter(final1, variable == c("tBodyAcc-std()-Y.x" , 
                                        "tBodyAcc-std()-Y.y" ) )
final66 <- group_by(final66, activity )
out <- summarise(final66,  "tBodyAcc-std()-Y" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-std()-Y-activity.txt", sep = ",", 
            col.names = NA, qmethod = "double")
final77 <- filter(final1, variable == c("tBodyAcc-std()-Z.x" , 
                                        "tBodyAcc-std()-Z.y" ) )
final77 <- group_by(final77, activity )
out <- summarise(final77,  "tBodyAcc-std()-Z" = mean(value, na.rm = TRUE))
write.table(out, file = "tBodyAcc-std()-Z-activity.txt", sep = ",", 
            col.names = NA, qmethod = "double")

