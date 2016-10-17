library("DMwR")
data(algae)
head(algae)
summary(algae)
str(algae)
algaeClean<-algae[-manyNAs(algae),0]
