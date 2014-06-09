
require(randomForest)

training <- read.table(
    "../RawData/Owls/training.csv", sep=",", header=TRUE)

model1 <- randomForest(kind   ~ ., training, ntree=10, do.trace=TRUE)
model2 <- randomForest(weight ~ ., training, ntree=3, do.trace=TRUE)

test <- read.table("../RawData/Owls/test.csv", sep=",", header=TRUE)

# Predict the kind
prediction1 <- predict(model1, test)
# Predict the weight
prediction2 <- predict(model2, test)


# Make a test set with predicted values, so we can make density plots.
#
test1 <- test
test1$kind <- prediction1

write.table(test1, file="test1.csv", sep=",", row.names=FALSE, quote=FALSE)

