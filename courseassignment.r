setwd("C:/Users/YuHeng/Google Drive/GitHub/Practical-Machine-Learning/assessment")

library(caret)
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(trainURL, na.strings=c("NA","#DIV/0!"))[,-1]
testing <- read.csv(testURL, na.strings=c("NA","#DIV/0!"))[,-1]

mostlyNA <- sapply(training, function(x) sum(is.na(x))/length(x))

train <- training[,mostlyNA<0.97][,-(1:6)]
test <- testing[,mostlyNA<0.97][,-(1:6)]

nzv <- nearZeroVar(train, saveMetrics=T)


time <- list()
modelfit <- list()
set.seed(1234)
time[[1]] <- system.time(modelfit[[1]] <- train(classe~., data=train, method="rf"))
set.seed(1234)
time[[2]] <- system.time(modelfit[[2]] <- train(classe~., data=train, method="gbm", verbose=F))
set.seed(1234)
time[[3]] <- system.time(modelfit[[3]] <- train(classe~., data=train, method="lda"))
set.seed(1234)
time[[4]] <- system.time(modelfit[[4]] <- train(classe~., data=train, method="qda"))
set.seed(1234)
time[[5]] <- system.time(modelfit[[5]] <- train(classe~., data=train, method="nb"))

accuracy <- numeric(5)
for (i in 1:5){
    accuracy[i] <- max(modelfit[[i]]$results$Accuracy)
}
results <- data.frame(accuracy, error=1-accuracy)
row.names(results) <- c("RF","GBM","LDA","QDA","NB")
results

modelfit[[1]]$finalModel

prediction <- predict(modelfit[[1]]$finalModel,test)
prediction

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(prediction)
