library(data.table)
library(recosystem) 
library(dplyr)

setwd("/Users/shengchenhao/Documents/GitHub/Recommandation-System")

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

# I need to create numeric key for user_id and item_id in train and test data 
# because the recosystem only support numeric (int) key

userid_train = unique(ratings$user) 
itemid_train = unique(ratings$item)
userid_test = unique(test$reviewerID) 
itemid_test = unique(test$asin)

# create numeric key for user and item
user = data.frame(user = unique(c(userid_test, userid_train)), userid = c(1:123960)) # number of unique user id in train and test data 
item = data.frame(item = unique(c(itemid_test, itemid_train)), itemid = c(1:51744)) # number of unique item id in train and test data 
ratings = merge(ratings, user, by = "user", all.x = T)
ratings = merge(ratings, item, by = "item", all.x = T)

train = dplyr::select(ratings, userid, itemid, rating)
train = as.data.table(train)


####################################################################
# RMSE
# in this part I split the train data into train (80%) and test (20%) to test the performance of model 
set.seed(1)
in_train <- rep(TRUE, nrow(train))
in_train[sample(1:nrow(train), size = round(0.2 * length(unique(train$user)), 0) * 5)] <- FALSE

ratings_train <- train[(in_train)]
ratings_test <- train[(!in_train)]

write.table(ratings_train, file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
write.table(ratings_test, file = "testset.txt", sep = " ", row.names = FALSE, col.names = FALSE)

r = Reco()

#opts <- r$tune("trainset.txt", opts = list(dim = c(10,20,30,40,50,60),nthread = 3, niter = 20, nfold = 3, verbose = FALSE))

r$train("trainset.txt", opts = c(dim = 20, lrate = 0.04, costp_l2 = 0.1,costq_l2 = 0.1, costp_l1 = 0,costq_l1 = 0,nthread = 3, niter = 40, nbin = 20,verbose = F))

outfile = tempfile()

r$predict("testset.txt", outfile)

scores_real <- read.table("testset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(outfile)
sqrt(mean((scores_real-scores_pred) ^ 2))


####################################################################
# using predicted test data in train 
temp = data.frame(rating = round(scan(outfile)))
temp_test = ratings_test
temp_test$rating = NULL
temp_train = rbind(ratings_train, cbind(temp_test, temp))
write.table(temp_train, file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
r = Reco()

r$train("trainset.txt", opts = c(dim = 15, lrate = 0.04, costp_l2 = 0.1,costq_l2 = 0.1,nthread = 3, niter = 40, nbin = 50,verbose = F))
outfile = tempfile()

r$predict("testset.txt", outfile)
scores_real <- read.table("testset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(outfile)
sqrt(mean((scores_real-scores_pred) ^ 2))
########################################
# train the full model 
# similar to the method above, except I'm using the whole train data to tune variables and perdicting

set.seed(1)
in_train <- rep(TRUE, nrow(train))
ratings_train <- train[(in_train)]
write.table(ratings_train, file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)

# tune variables 
r = Reco()
opts <- r$tune("trainset.txt", opts = list(dim = c(5,10,15,20),nthread = 3, niter = 20, nfold = 5, nbin = 20, verbose = FALSE))
r$train("trainset.txt", opts = c(opts$min, nthread = 3, niter = 2000, nbin = 20,verbose = FALSE)) # use the opt variables 
outfile = tempfile()


# predict the real test data 
temp = test
temp$datapointID = NULL 
colnames(temp) = c("user", "item")
temp = merge(temp, user, by = "user", all.x = T) 
temp = merge(temp, item, by = "item", all.x = T)
temp$item = NULL 
temp$user = NULL

write.table(temp, file = "test.txt", sep = " ", row.names = FALSE, col.names = FALSE)
r$predict("test.txt", outfile)

scores_pred <- scan(outfile)
temp = cbind(temp, data.frame(score = scores_pred))

test = merge(test, user, all.x = T, by.x = "reviewerID", by.y = "user") 
test = merge(test, item, all.x = T, by.x = "asin", by.y = "item")
test = merge(test, temp, by.x = c("userid", "itemid"), by.y = c("userid", "itemid"))


# output 
output = dplyr::select(test, datapointID, score)

colnames(output) = c("datapointID", "overall")
write.csv(output,file="submitfile.csv", row.names = F)




