
rm(list=ls())

load("../RawData/Owls/dataset.rdata")
full_training <- dataset[!dataset$test,]
full_testset  <- dataset[dataset$test,]

exp1  <- kind ~ gender + height + weight

model      <- glm(exp1, full_training, family = "binomial")
prediction <- predict(model, full_testset, type="response")

predicted_kind <- factor(prediction>0.5)
levels(predicted_kind) <- levels(full_testset$kind)
stats <- table(predicted_kind, full_testset$kind)

