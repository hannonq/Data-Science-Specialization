library(data.table)
library(dplyr)
library(matrixStats)

analysis <- function(){
    test.subject_test <- fread('./Dataset/test/subject_test.txt')
    test.X_test <- fread('./Dataset/test/X_test.txt')
    test.Y_test <- fread('./Dataset/test/y_test.txt')
    
    train.subject_train <- fread('./Dataset/train/subject_train.txt')
    train.X_train <- fread('./Dataset/train/X_train.txt')
    train.Y_train <- fread('./Dataset/train/y_train.txt')
    
    test <- cbind(test.subject_test, test.Y_test, test.X_test)
    train <- cbind(train.subject_train, train.Y_train, train.X_train)
    
    colnames(test)[1:2] <- c("Subject", "Activity")
    colnames(train)[1:2] <- c("Subject", "Activity")
    
    data <- rbind(test, train)
    data$Activity <- sapply(data$Activity, function(x){
        if(x==1) return ("Walking")
        else if(x==2) return ("Walking Upstairs")
        else if(x==3) return ("Walking Downstairs")
        else if(x==4) return ("Sitting")
        else if(x==5) return ("Standing")
        else if(x==6) return ("Laying")
        else return (NA)
    })
    data$mean <- rowMeans(select(data, V1:V561))
    data$`standard deviation` <- rowSds(as.matrix(select(data, V1:V561)), na.rm=TRUE)
    print(select(data, `standard deviation`))
}

analysis()