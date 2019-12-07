rm(list=ls())

library(readxl)
AirBnBNYC <- read_excel("AirBnBNYC.xlsx")

# Data Cleanup

data = AirBnBNYC
data$neighbourhood = factor(data$neighbourhood)
data$neighbourhood_group = factor(data$neighbourhood_group)
data$reviews_per_month[is.na(data$reviews_per_month)] = 0

# data$neighbourhood <- to_categorical(as.numeric(as.factor(data$neighbourhood)) - 1)
# data$neighbourhood_group <- to_categorical(as.numeric(as.factor(data$neighbourhood_group)) - 1)

set.seed(1)
pd = sample(2,nrow(data),replace = TRUE, prob = c(0.8,0.2))
train = data[pd==1,]
test = data[pd==2,]

# Decision Tree

library(tree)
tree = tree(price ~ neighbourhood_group + number_of_reviews + 
              minimum_nights + reviews_per_month + availability_365,
            data = train)
tree.train_mse = mean(residuals(tree)^2)
tree.test_mse = mean((predict(tree, test) - test$price)^2)

plot(tree)
text(tree,cex=.7)

# Decision Tree Using RPart

library(rpart)
rtree = rpart(price ~ neighbourhood_group + number_of_reviews + 
                minimum_nights + reviews_per_month + availability_365,
              data = train)
rtree.train_mse = mean(residuals(rtree)^2)
rtree.test_mse = mean((predict(rtree, test) - test$price)^2)

library(rpart.plot)
rpart.plot(rtree)

# Bagging

library(randomForest)
set.seed(1)
bagging = randomForest(price ~ neighbourhood_group + number_of_reviews + 
                         minimum_nights + reviews_per_month + availability_365,
                       mtry = 5,
                       data = train)
bagging.train_mse = mean((predict(bagging) - train$price)^2)
bagging.test_mse = mean((predict(bagging, test) - test$price)^2)
importance(bagging)

# Random Forest

set.seed(1)
rf = randomForest(price ~ neighbourhood_group + number_of_reviews + 
                    minimum_nights + reviews_per_month + availability_365,
                  data = train)
rf.train_mse = mean((predict(rf) - train$price)^2)
rf.test_mse = mean((predict(rf, test) - test$price)^2)
importance(rf)
