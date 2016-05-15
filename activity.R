
## ----cache=TRUE----------------------------------------------------------
library(RCurl)
training_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training_data <- read.csv(text=getURL(training_url), na.strings=c("", "NA"))
test_data <- read.csv(text=getURL(test_url), na.strings=c("", "NA"))


## ------------------------------------------------------------------------
training_data$X <- NULL


## ------------------------------------------------------------------------
cols_to_remove <- c("user_name", "raw_timestamp_part_1",
                    "raw_timestamp_part_2", "cvtd_timestamp")
for (col in cols_to_remove) {
    training_data[, col] <- NULL
}


## ------------------------------------------------------------------------
NAs <- apply(training_data,2,function(x) {sum(is.na(x))})
training_data <- training_data[,which(NAs == 0)]


## ----message=FALSE-------------------------------------------------------
library(caret)
nsv <- nearZeroVar(training_data)
training_data <- training_data[-nsv]
test_data <- test_data[-nsv]


## ------------------------------------------------------------------------
names(training_data)


## ----cache=TRUE----------------------------------------------------------
library(randomForest)
set.seed(1)
obs <- c()
preds <- c()
for(i in 1:10) {
    intrain = sample(1:dim(training_data)[1], size=dim(training_data)[1] * 0.8, replace=F)
    train_cross = training_data[intrain,]
    test_cross = training_data[-intrain,]
    rf <- randomForest(classe ~ ., data=train_cross)
    obs <- c(obs, test_cross$classe)
    preds <- c(preds, predict(rf, test_cross))
}


## ------------------------------------------------------------------------
conf_mat <- confusionMatrix(table(preds, obs))
conf_mat$table


## ----cache=TRUE----------------------------------------------------------
model <- randomForest(classe ~ ., data=training_data)


