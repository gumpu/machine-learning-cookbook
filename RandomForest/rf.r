# vi: spell spl=en
#
require(randomForest)

# > bitAnd(c(1,2,4,8),2)>0
# [1] FALSE  TRUE FALSE FALSE

training <- read.table(
    "../RawData/Owls/training.csv", sep=",", header=TRUE)

model1 <- randomForest(
    kind ~ gender + height + weight + wingspan, 
    training, ntree=20, do.trace=TRUE)

model2 <- randomForest(
    kind ~ age + gender + height + weight + wingspan, 
    training, ntree=20, do.trace=TRUE)

model3 <- randomForest(
    kind ~ gender + height + weight, 
    training, ntree=20, do.trace=TRUE)

model1
model2
model3

# model2 <- randomForest(weight ~ ., training, ntree=3, do.trace=TRUE)

test <- read.table("../RawData/Owls/test.csv", sep=",", header=TRUE)

# Predict the kind
prediction1 <- predict(model1, test)
prediction2 <- predict(model2, test)
prediction3 <- predict(model3, test)

# Make a test set with predicted values, so we can make density plots.

table(prediction1, test$kind)
table(prediction2, test$kind)
table(prediction3, test$kind)

test1      <- test
test1$kind <- prediction3
write.table(test1, file="test1.csv", sep=",", row.names=FALSE, quote=FALSE)


