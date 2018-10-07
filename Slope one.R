# Slope one #########
library(SlopeOne)

ratings = read.csv(file = "training.csv", header = T, sep = ",") 
test = read.csv(file="reviews.test.unlabeled.csv", header=TRUE, sep=",")

ratings$X = NULL 
colnames(ratings) = c("user", "item", "rating")
ratings = as.data.table(ratings) 

ratings = mutate(ratings, user = as.character(user), 
                 item = as.character(item)) 
test = mutate(test, datapointID = as.character(datapointID), 
              reviewerID = as.character(reviewerID), 
              asin = as.character(asin))

# create numeric key for user and item
userid_train = unique(ratings$user) 
itemid_train = unique(ratings$item)
userid_test = unique(test$reviewerID) 
itemid_test = unique(test$asin)

user = data.frame(user = unique(c(userid_test, userid_train)), userid = c(1:123960)) 
item = data.frame(item = unique(c(itemid_test, itemid_train)), itemid = c(1:51744))
ratings = merge(ratings, user, by = "user", all.x = T)
ratings = merge(ratings, item, by = "item", all.x = T)

train = dplyr::select(ratings, userid, itemid, rating)
train = as.data.table(train)

test = merge(test, user, all.x = T, by.x = "reviewerID", by.y = "user") 
test = merge(test, item, all.x = T, by.x = "asin", by.y = "item")
test = data.table(test)

############################## load data finish ###################################
names(train) <- c("user_id", "item_id", "rating")
train <- data.table(train)
colnames(test)[4] = "user_id" 
colnames(test)[5] = "item_id" 

train[, user_id := as.character(user_id)]
train[, item_id := as.character(item_id)]
test[, user_id := as.character(user_id)]
test[, item_id := as.character(item_id)]

setkey(train, user_id, item_id)

# get the train data (train all whole dataset)
set.seed(1)
in_train <- rep(TRUE, nrow(train))
ratings_train <- train[(in_train)]

ratings_train_norm <- normalize_ratings(ratings_train)
model <- build_slopeone(ratings_train_norm$ratings)


predictions <- predict_slopeone(model, 
                                test[ , c(4, 5), with = FALSE], 
                                ratings_train_norm$ratings)
unnormalized_predictions <- unnormalize_ratings(normalized = ratings_train_norm, 
                                                ratings = predictions)

# 
