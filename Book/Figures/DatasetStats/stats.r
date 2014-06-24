load("../../../RawData/Owls/dataset.rdata")

# pdf(file="age_table.pdf")
# plot(table(dataset$age))
# dev.off()

pdf.options(family = "Bookman", width=4, height=4)
pdf(file="age_density.pdf")
plot(density(dataset$age), xlab="Age", main=NA)
dev.off()

pdf(file="height_density.pdf")
plot(density(dataset$height), xlab="Height", main=NA)
dev.off()

pdf(file="weight_density.pdf")
plot(density(dataset$weight), xlab="Weight", main=NA)
dev.off()



