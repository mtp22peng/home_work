
# section 0
library(dplyr)
library(tidyr)

c0 <- read.table("./UCI HAR Dataset/activity_labels.txt",stringsAsFactors=FALSE )
c1 <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors=FALSE )


# section 1 
d1 <- read.table("./UCI HAR Dataset/train/X_train.txt")
d22 <- read.table("./UCI HAR Dataset/train/subject_train.txt")
d22 <- mutate(d22, subject2 = paste("subject", d22[,1]))
names(d22)[1]<-"subject1"
names(d22)[2]<-"subject"
d2 <- select(d22, subject)
d3 <- read.table("./UCI HAR Dataset/train/Y_train.txt")
names(d3)[1]<-"activity"
d4 <- c0[d3[,1],2]


# section 2
c2 <- select(d1,   V1: V6)
names(c2)[1:6]<-c1[1:6,2]
fin1 <- cbind(d2,d4,c2)
names(fin1)[2] <- "activity"
rm("d1")
rm("d2")
rm("d22")
rm("d3")
rm("d4")


# section 3
e1 <- read.table("./UCI HAR Dataset/test/X_test.txt")
e22 <- read.table("./UCI HAR Dataset/test/subject_test.txt")
e22 <- mutate(e22, subject2 = paste("subject", e22[,1]))
names(e22)[1]<-"subject1"
names(e22)[2]<-"subject"
e2 <- select(e22, subject)
e3 <- read.table("./UCI HAR Dataset/test/Y_test.txt")
names(e3)[1]<-"activity"
e4 <- c0[e3[,1],2]


# section 4
f2 <- select(e1,   V1: V6)
names(f2)[1:6]<-c1[1:6,2]
fin2 <- cbind(e2,e4,f2)
names(fin2)[2] <- "activity"
rm("e1")
rm("e2")
rm("e22")
rm("e3")
rm("e4")


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
out2 <- summarise(final2,  "tBodyAcc-mean()-X" = mean(value, na.rm = TRUE))

final3 <- filter(final1, variable == c("tBodyAcc-mean()-Y.x" , 
                                       "tBodyAcc-mean()-Y.y" ) )
final3 <- group_by(final3, subject )
out3 <- summarise(final3,  "tBodyAcc-mean()-Y" = mean(value, na.rm = TRUE))
final4 <- filter(final1, variable == c("tBodyAcc-mean()-Z.x" , 
                                       "tBodyAcc-mean()-Z.y" ) )
final4 <- group_by(final4, subject )
out4 <- summarise(final4,  "tBodyAcc-mean()-Z" = mean(value, na.rm = TRUE))
final5 <- filter(final1, variable == c("tBodyAcc-std()-X.x" , 
                                       "tBodyAcc-std()-X.y" ) )
final5 <- group_by(final5, subject )
out5 <- summarise(final5,  "tBodyAcc-std()-X" = mean(value, na.rm = TRUE))
final6 <- filter(final1, variable == c("tBodyAcc-std()-Y.x" , 
                                       "tBodyAcc-std()-Y.y" ) )
final6 <- group_by(final6, subject )
out6 <- summarise(final6,  "tBodyAcc-std()-Y" = mean(value, na.rm = TRUE))
final7 <- filter(final1, variable == c("tBodyAcc-std()-Z.x" , 
                                       "tBodyAcc-std()-Z.y" ) )
final7 <- group_by(final7, subject )
out7 <- summarise(final7,  "tBodyAcc-std()-Z" = mean(value, na.rm = TRUE))

out8 <- full_join(out2, out3, by = "subject") %>% 
        full_join(out4, by = "subject") %>% 
        full_join(out5, by = "subject") %>% 
        full_join(out6, by = "subject") %>% 
        full_join(out7, by = "subject") 
out9 <- gather(out8, key = "variable" , value = "value", -c(1))


# section 7
# activity
final22 <- filter(final1, variable == c("tBodyAcc-mean()-X.x" , 
                                        "tBodyAcc-mean()-X.y" ) )
final22 <- group_by(final22, activity )
out22 <- summarise(final22,  "tBodyAcc-mean()-X" = mean(value, na.rm = TRUE))
final33 <- filter(final1, variable == c("tBodyAcc-mean()-Y.x" , 
                                        "tBodyAcc-mean()-Y.y" ) )
final33 <- group_by(final33, activity )
out33 <- summarise(final33,  "tBodyAcc-mean()-Y" = mean(value, na.rm = TRUE))
final44 <- filter(final1, variable == c("tBodyAcc-mean()-Z.x" , 
                                        "tBodyAcc-mean()-Z.y" ) )
final44 <- group_by(final44, activity )
out44 <- summarise(final44,  "tBodyAcc-mean()-Z" = mean(value, na.rm = TRUE))
final55 <- filter(final1, variable == c("tBodyAcc-std()-X.x" , 
                                        "tBodyAcc-std()-X.y" ) )
final55 <- group_by(final55, activity )
out55 <- summarise(final55,  "tBodyAcc-std()-X" = mean(value, na.rm = TRUE))
final66 <- filter(final1, variable == c("tBodyAcc-std()-Y.x" , 
                                        "tBodyAcc-std()-Y.y" ) )
final66 <- group_by(final66, activity )
out66 <- summarise(final66,  "tBodyAcc-std()-Y" = mean(value, na.rm = TRUE))
final77 <- filter(final1, variable == c("tBodyAcc-std()-Z.x" , 
                                        "tBodyAcc-std()-Z.y" ) )
final77 <- group_by(final77, activity )
out77 <- summarise(final77,  "tBodyAcc-std()-Z" = mean(value, na.rm = TRUE))
out88 <- full_join(out22, out33, by = "activity") %>% 
        full_join(out44, by = "activity") %>% 
        full_join(out55, by = "activity") %>% 
        full_join(out66, by = "activity") %>% 
        full_join(out77, by = "activity") 


# section 8
out <- full_join(out8, out88)
out10 <-  gather(out, key = "study" , value = "id", -c(2:7))
out11 <- gather(out10, key = "variable" , value = "value", -c(7, 8))
write.table(out11, file = "out.txt", sep = ",", 
            col.names = NA, qmethod = "double")
