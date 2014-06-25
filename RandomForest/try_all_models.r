# > bitAnd(c(1,2,4,8),2)>0
# [1] FALSE  TRUE FALSE FALSE

require(bitops)
load("../RawData/Owls/dataset.rdata")

selection <- function(n, x) {
    twopowers <- 2^(0:(n-1))
    bitAnd(twopowers,x)>0
}

features <- c("gender", "eyesize", "headsize", 
              "age", "height",  "wingspan", "weight")


n <- length(features)

for (i in 1:(2^n-1)) {
    print(features[selection(n, i)])
}


# Create a training subset

# Create estimator

# Compute Score

