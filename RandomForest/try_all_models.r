require(bitops)
require(pROC)
require(randomForest)
set.seed(196711)

# This prevents many errors...
rm(list=ls())

# There are 2^n ways to select m items from n items.
# This function gives the x-th way in the form
# of a vector of logicals.
select <- function(n, x) {
    twopowers <- 2^(0:(n-1))
    # The trick:
    # > bitAnd(c(1,2,4,8),2)>0
    # [1] FALSE  TRUE FALSE FALSE
    bitAnd(twopowers, x) > 0
}

#===========================================================================
get_metrics <- function(fit, selection, testdata, kind, model_number) {
    # Predict the labels for the testset
    prediction <- predict(fit, testdata, type="prob")

    # Compute Score
    r <- roc(testdata$kind, prediction[,'city'])
    # Area under the curve
    auc <- r$auc

    # Type I and II errors etc
    prediction <- predict(fit, testdata, type="response")
    tt <- table(prediction, testdata$kind)
    forest.forest     <- tt['forest', 'forest']
    forest.but.city   <- tt['forest', 'city']
    city.but.forest   <- tt['city', 'forest']
    city.city         <- tt['city', 'city']

    # Lets take city as being the positive

    # number of true positives (i.e. number of items correctly labeled
    # as belonging to the positive class) divided by the total
    # number of elements labeled as belonging to the positive class
    # [1]
    precision <- city.city / (city.city + city.but.forest)

    # Recall is defined as the number of true positives divided by the
    # total number of elements that actually belong to the positive
    # class (i.e. the sum of true positives and false negatives, which
    # are items which were not labeled as belonging to the
    # positive class but should have been). [1]
    recall    <- city.city / (city.city + forest.but.city)

    error_rate <- (forest.but.city + city.but.forest)/nrow(testdata)

    oob_error_rate <- fit$err.rate[nrow(fit$err.rate),'OOB']
    # Put it all into a dataframe
    return(
        data.frame(model = paste(selection, collapse="+"),
                   auc=auc,
                   forest.forest   = forest.forest,
                   forest.but.city = forest.but.city,
                   city.but.forest = city.but.forest,
                   city.city       = city.city,
                   precision       = precision,
                   recall          = recall,
                   error_rate      = error_rate,
                   oob_error_rate  = oob_error_rate,
                   kind            = kind,
                   model_number    = model_number)
    )
}

#===========================================================================
try_all_models <- function(features, training, testset) {
    n <- length(features)
    performance <- data.frame()
    for (i in 1:(2^n-1)) {
        print(i)
        # Create a training subset
        selection <- features[select(n, i)]

        if (length(selection) > 1) {
            # Create estimator
            fit <- randomForest(y=training$kind, 
                                x=training[,selection],
                                ntree=120)

            # Evaluate the performance
            performance <- rbind( performance, 
                get_metrics(fit, selection, testset, "test", i))
            performance <- rbind(performance, 
                get_metrics(fit, selection, training, "train", i))

        }
    }
    return(performance)
}


load("../RawData/Owls/dataset.rdata")
full_training <- dataset[!dataset$test,]
full_testset  <- dataset[dataset$test,]

features <- c("gender", "eyesize", "headsize", 
              "age", "height",  "wingspan", "weight")


options(digits=6)

#=============================================================================
perf <- try_all_models(features, full_training, full_testset)
write.csv(file="all_models_perf_full_sets.csv", perf, row.names=FALSE)

#=============================================================================
idx <- sample.int(nrow(dataset), 200)
small_dataset  <- dataset[idx,]
small_training <- small_dataset[!small_dataset$test,]
small_testset  <- small_dataset[small_dataset$test,]
perf_small <- try_all_models(features, small_training, small_testset)
write.csv(file="all_models_perf_small_sets.csv", perf_small, row.names=FALSE)

#=============================================================================
idx <- sample.int(nrow(dataset), 200)
small_dataset  <- dataset[idx,]
small_training <- small_dataset[!small_dataset$test,]
perf_small <- try_all_models(features, small_training, full_testset)
write.csv(file="all_models_perf_small_train.csv", perf_small, row.names=FALSE)


#=============================================================================
# [1] http://en.wikipedia.org/wiki/Precision_and_recall

