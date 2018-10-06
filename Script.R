library(recommenderlab)
library(reshape2)
library(Matrix)


#train_data = stream_in(file("reviews.training.json"))
#save(train_data, file = "Train.RData")

load("Train.RData")
train_data = dplyr::select(train_data, reviewerID, asin, overall) 
write.csv(train_data, file = "training.csv")
######################
train_data = read.csv(file = "training.csv", header = T, sep = ",") 
train_data$X = NULL
test = read.csv(file="reviews.test.unlabeled.csv", header=TRUE, sep=",")


tr<-read.csv("train_v2.csv",header=TRUE)
tr<-tr[,-c(1)]

#train_data$helpful = NULL
#write.csv2(train_data, file = "train.csv")

colnames(train_data) = colnames(tr) 
rm(tr) 
gc()
g<-acast(train_data, user ~ movie)
# Check the class of g

# Convert it as a matrix
R<-as.matrix(g)

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(data.sparse, "realRatingMatrix")
# normalize the rating matrix
r_m <- normalize(r)

# Can also turn the matrix into a 0-1 binary matrix
r_b <- binarize(r, minRating=1)

rec=Recommender(r[1:nrow(r)],method="POPULAR")

recom <- predict(rec, r[1:nrow(r)], type="ratings") 
rec_list<-as(recom,"list")

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

save.image("output.RData")

# test for sparse matrix 
data.sparse = sparseMatrix(as.integer(train_data$user), as.integer(train_data$movie), x = train_data$rating)

colnames(data.sparse) = levels(train_data$movie)
rownames(data.sparse) = levels(train_data$user)



