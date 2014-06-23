
require(ggplot2)

arguments <- commandArgs(trailingOnly=TRUE)

dataset1_filename   <- arguments[1]
dataset2_filename   <- arguments[2]
class_variable_name <- arguments[3]

print(c(dataset1_filename, dataset2_filename, class_variable_name))

training <- read.table(dataset1_filename, sep=",", header=TRUE)
test     <- read.table(dataset2_filename, sep=",", header=TRUE)

training$set <- "train"
test$set     <- "prediction"

dataset <- rbind(training, test)
dataset$set <- factor(dataset$set)

str(dataset)
factors <- c("age", "height", "wingspan", "weight", "headsize", "eyesize")
pdf("density_plot.pdf")
for (f in factors) {
    p1 <- ggplot(dataset[,c(f,"set","kind")], aes_string(x=f)) + 
        geom_density(aes(color=set))+facet_grid(~kind)
    print(p1)
}
dev.off()


