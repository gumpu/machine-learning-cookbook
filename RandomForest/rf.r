# vi: spell spl=en
#
require(randomForest)

# > bitAnd(c(1,2,4,8),2)>0
# [1] FALSE  TRUE FALSE FALSE

training <- read.table(
    "../RawData/Owls/training.csv", sep=",", header=TRUE)

fit1 <- randomForest(
    kind ~ gender + height + weight + wingspan, 
    training, ntree=20, do.trace=TRUE)

fit2 <- randomForest(
    kind ~ age + gender + height + weight + wingspan, 
    training, ntree=20, do.trace=TRUE)

fit3 <- randomForest(
    kind ~ age+height+wingspan+weight,
    # kind ~ gender + height + weight, 
    training, ntree=120, do.trace=TRUE)

fit1
fit2
fit3

# fit2 <- randomForest(weight ~ ., training, ntree=3, do.trace=TRUE)

test <- read.table("../RawData/Owls/test.csv", sep=",", header=TRUE)

# Predict the kind
prediction1 <- predict(fit1, test)
prediction2 <- predict(fit2, test)
prediction3 <- predict(fit3, test)

# Make a test set with predicted values, so we can make density plots.

table(prediction1, test$kind)
table(prediction2, test$kind)
table(prediction3, test$kind)

test1      <- test
test1$kind <- prediction3
write.table(test1, file="test1.csv", sep=",", row.names=FALSE, quote=FALSE)


# plot(labels(model3$err.rate)[[1]],model3$err.rate[,1],type="b")

