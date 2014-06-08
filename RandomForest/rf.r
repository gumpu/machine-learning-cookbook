
require(randomForest)

training <- read.table(
    "../RawData/Owls/training.csv", sep=",", header=TRUE)

model1 <- randomForest(kind   ~ ., training, ntree=10, do.trace=TRUE)
model2 <- randomForest(weight ~ ., training, ntree=3, do.trace=TRUE)

test <- read.table(
    "../RawData/Owls/test.csv", sep=",", header=TRUE)

prediction1 <- predict(model1, test)
prediction2 <- predict(model2, test)

