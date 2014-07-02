# > bitAnd(c(1,2,4,8),2)>0
# [1] FALSE  TRUE FALSE FALSE

require(bitops)
require(pROC)
require(randomForest)

load("../RawData/Owls/dataset.rdata")

training <- dataset[!dataset$test,]
testset  <- dataset[dataset$test,]

select <- function(n, x) {
    twopowers <- 2^(0:(n-1))
    bitAnd(twopowers,x)>0
}

features <- c("gender", "eyesize", "headsize", 
              "age", "height",  "wingspan", "weight")


n <- length(features)

preformace <- data.frame()

for (i in 1:(2^n-1)) {
    # Create a training subset
    selection <- features[select(n, i)]

    if (length(selection) > 1) {
        print(selection)
        # Create estimator
        model <- randomForest(y=training$kind, 
                              x=training[,selection],
                              ntree=60,
                              do.trace=TRUE)

        # Predict the labels for the testset
        prediction <- predict(model, testset, type="prob")

        # Compute Score
        r <- roc(testset$kind, prediction[,'city'])
        plot(r)
        # Area under the curve
        auc <- r$auc

        # Type I and II errors etc
        prediction <- predict(model, testset, type="response")
        tt <- table(prediction, testset$kind)
        forest.forest     <- tt['forest', 'forest']
        forest.but.city   <- tt['forest', 'city']
        city.but.forest   <- tt['city', 'forest']
        forest.forest     <- tt['forest', 'forest']
    }
}








