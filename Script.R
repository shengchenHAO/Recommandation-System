library(jsonlite)
library(methods)
library(recommenderlab)
library(data.table)
library(ggplot2)
library(knitr)


train_data = stream_in(file("reviews.training.json"))
save(train_data, file = "Train.RData")

load("Train.RData")
train_data$reviewerName = NULL
train_data$reviewText = NULL
test_data = read.csv(file="reviews.test.unlabeled.csv", header=TRUE, sep=",")

#train_data$helpful = NULL
#write.csv2(train_data, file = "train.csv")


