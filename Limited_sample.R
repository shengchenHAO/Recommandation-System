library(recommenderlab)
library(reshape2)
library(Matrix)
library(dplyr)

train_data = read.csv(file = "training.csv", header = T, sep = ",") 
train_data$X = NULL 

tr<-read.csv("train_v2.csv",header=TRUE)
tr<-tr[,-c(1)] 

colnames(train_data) = colnames(tr) 
rm(tr) 
gc()

train_data = dplyr::mutate(train_data, temp = 1) %>% 
  arrange(movie,user) %>% 
  group_by(movie) %>%
  mutate(count = cumsum(temp))

train_data$temp = NULL


temp1 = dplyr::filter(train_data, count == 1)
temp1$count = NULL 
temp1$temp = NULL

g<-acast(temp1, user ~ movie)

R<-as.matrix(g)
rm(g, temp1, train_data)
r <- as(R, "realRatingMatrix")

rec=Recommender(r[1:nrow(r)],method="POPULAR")

recom <- predict(rec, r[1:nrow(r)], type="ratings")
rec_list<-as(recom,"list")


test = read.csv(file="reviews.test.unlabeled.csv", header=TRUE, sep=",")
ratings<-NULL
# For all lines in test file, one by one
for ( u in 1:length(test[,2]))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- test[u,2]
  movieid<-test[u,3]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(test[,1],round(ratings))

save.image("limited_sample.RData")