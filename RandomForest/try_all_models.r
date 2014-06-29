# > bitAnd(c(1,2,4,8),2)>0
# [1] FALSE  TRUE FALSE FALSE

require(bitops)
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
    selection <- features[select(n, i)]
    if (length(selection) > 1) {
        print(selection)
        model <- randomForest(y=training$kind, 
                              x=training[,selection],
                              ntree=20,
                              do.trace=TRUE)
    }
}


# Create a training subset

# Create estimator

# Compute Score

