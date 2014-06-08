# vi: spell spl=en

set.seed(76514345)
rm(list=ls())

number_of_owls    <- 50000+50000

# kind,  forest, meadow city, 
kind        <- sample(c(2,9),number_of_owls, replace=TRUE)
kind_factor <- log10(kind)
kind        <- factor(kind)
levels(kind) <- c("forest","city")

#=============================================================================
forest   <- ifelse(runif(number_of_owls)<0.55, 0, 1)
gender   <- ifelse(runif(number_of_owls)<0.45, 0, 1)
age      <- 2+rexp(number_of_owls,rate=0.15)+(60*runif(number_of_owls)+1)
age      <- floor(age)
height   <- 50 - kind_factor*15 + gender*10 + rnorm(number_of_owls, mean=0,sd=3)
height   <- floor(height + 7*log(age))
eyesize  <- rnorm(number_of_owls,mean=20,sd=0.2)
headsize <- 3*eyesize + rnorm(number_of_owls,mean=1,sd=0.2)
wingspan <- 70 + gender*14 + rnorm(number_of_owls, mean=0,sd=5)
wingspan <- floor(wingspan + 7.5*log(age))
weight   <- floor(height*wingspan/3 - 10*rnorm(number_of_owls,sd=1) + 150*kind_factor)

gender         <- factor(gender)
levels(gender) <- c("female", "male")

dataset <- data.frame(
    gender=gender,
    eyesize=eyesize,
    headsize=headsize,
    age=age,
    height=height,
    wingspan=wingspan,
    weight=weight,
    kind=kind
)
options(digits=4)

# 5000 test samples,
# 5000 training samples
ind <- sample.int(nrow(dataset), 50000)
trainingset <- dataset[ ind,]
testset     <- dataset[-ind,]

write.table(trainingset,
    file="training.csv", sep=",", quote=FALSE,
    row.names=FALSE, col.names=TRUE)
save(trainingset, file="training.rdata")

write.table(testset,
    file="test.csv", sep=",", quote=FALSE,
    row.names=FALSE, col.names=TRUE)
save(testset, file="test.rdata")


# Same dataset but now with missing values
ind <- sample.int(nrow(dataset), 1500)
dataset[ind, "age"] <- NA
ind <- sample.int(nrow(dataset), 2000)
dataset[ind, "weight"] <- NA
ind <- sample.int(nrow(dataset), 2200)
dataset[ind, "gender"] <- NA

ind <- sample.int(nrow(dataset), 50000)
trainingset <- dataset[ ind,]
testset     <- dataset[-ind,]

write.table(trainingset,
    file="training_with_na.csv", sep=",", quote=FALSE,
    row.names=FALSE, col.names=TRUE)
save(trainingset, file="training.rdata")

write.table(testset,
    file="test_with_na.csv", sep=",", quote=FALSE,
    row.names=FALSE, col.names=TRUE)
save(testset, file="test.rdata")



