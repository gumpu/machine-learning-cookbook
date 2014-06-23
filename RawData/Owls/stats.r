load("dataset.rdata")

# pdf(file="age_table.pdf")
# plot(table(dataset$age))
# dev.off()

pdf(file="age_density.pdf")
plot(density(dataset$age), main="Age density")
dev.off()

