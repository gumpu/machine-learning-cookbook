# > bitAnd(c(1,2,4,8),2)>0
# [1] FALSE  TRUE FALSE FALSE

require(bitops)
require(pROC)
require(randomForest)
set.seed(196711)

load("../RawData/Owls/dataset.rdata")

training <- dataset[!dataset$test,]
testset  <- dataset[dataset$test,]

# There are 2^n ways to select m items from n items.
# This function gives the x-th way in the form
# of a vector of logicals.
select <- function(n, x) {
    twopowers <- 2^(0:(n-1))
    bitAnd(twopowers,x)>0
}

features <- c("gender", "eyesize", "headsize", 
              "age", "height",  "wingspan", "weight")


n <- length(features)

performance <- data.frame()

for (i in 1:(2^n-1)) {
    # Create a training subset
    selection <- features[select(n, i)]

    if (length(selection) > 1) {
        print(selection)
        # Create estimator
        model <- randomForest(y=training$kind, 
                              x=training[,selection],
                              ntree=120,
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
        city.city         <- tt['city', 'city']

        # Lets take city as being the positive

        # number of true positives (i.e. number of items correctly labeled as
        # belonging to the positive class) divided by the total number of
        # elements labeled as belonging to the positive class  [1]
        precision <- city.city / (city.city + city.but.forest)

        # Recall is defined as the number of true positives
        # divided by the total number of elements that actually belong to the
        # positive class (i.e. the sum of true positives and false negatives,
        # which are items which were not labeled as belonging to the
        # positive class but should have been). [1]
        recall    <- city.city / (city.city + forest.but.city)

        error_rate <- (forest.but.city + city.but.forest)/nrow(testset)

        # Add it to dataframe with performace data
        performance <- rbind(performance,
            data.frame(model = paste(selection, collapse="+"),
                       auc=auc,
                       forest.forest   = forest.forest,
                       forest.but.city = forest.but.city,
                       city.but.forest = city.but.forest,
                       city.city       = city.city,
                       precision       = precision,
                       recall          = recall,
                       error_rate      = error_rate )
        )
    }
}




# [1] http://en.wikipedia.org/wiki/Precision_and_recall



