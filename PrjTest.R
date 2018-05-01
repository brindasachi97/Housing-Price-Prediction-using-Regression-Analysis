library(readr)
df <- read.csv("/home/brinda/College /DataAnalytics/Project/kc_house_data.csv", stringsAsFactors = FALSE)
#View(housing)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
#library(doMC)
library(scales)
library(GGally)

#source("utils.R")
print(paste("rows:", nrow(df), "cols:", ncol(df)))
#Remove id and date columns and instruct R to interpret condition, view, grade and waterfront as factors.
df <- df[-c(1, 2)]
df$condition <- as.factor(df$condition)
df$view <- as.factor(df$view)
df$grade <- as.factor(df$grade)
df$waterfront <- as.factor(df$waterfront)

#ggplot_missing(df)

#distribution of house condtion, grade and price:
p1 <- qplot(condition, data=df, geom = "bar",
            main="Number of houses by condition")

p2 <- qplot(grade, data=df, geom = "bar",
            main="Number of houses by grade")

p3 <- ggplot(df, aes(price)) + geom_density() + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma, limits = c(0, 2e+06)) +
  xlab("price") +
  ggtitle("Price distribution")

#multiplot(p1, p2, p3)
plot(p1)
plot(p2)
plot(p3)

#price (log10) vs other features
#1.Sqft of Living Area
ggplot(df, aes(x=log10(price), y=sqft_living)) +
  geom_smooth() +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  ylab("sqft of living area") + 
  geom_point(shape=1, alpha=1/10) +
  ggtitle("Price (log10) vs sqft of living area")

#2.Grade
ggplot(df, aes(x=grade, y=log10(price))) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  geom_point(shape=1, alpha=1/10) +
  ggtitle("Price (log10) vs grade")

#3. Condition
ggplot(df, aes(x=condition, y=log10(price))) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  geom_point(shape=1, alpha=1/10) +
  ggtitle("Price (log10) vs condition")

#4. Number of Floors
ggplot(df, aes(x=as.factor(floors), y=log10(price))) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  xlab("floors") +
  coord_flip() +
  geom_point(shape=1, alpha=1/10) +
  ggtitle("Price (log10) vs number of floors")

#Feature Correlation
ggcorr(df, hjust = 0.8, layout.exp = 1) + 
  ggtitle("Correlation between house features")

#Splitting Data
train_idx = createDataPartition(df$price, p=.9, list=FALSE)
train <- df[train_idx, ]
test <- df[-train_idx, ]

#extract the labels (true values) from our test dataset
test_labels <- test[, 1] #actual

#Model1
tree_fit <- rpart(price ~ ., data=df) 
tree_predicted <- predict(tree_fit, test) #predicted
summary(tree_predicted)
summary(test_labels)
cor(tree_predicted, test_labels)
#rmse(tree_predicted, test_labels)
#Model 1:Predicted v/s Actual
res <- data.frame(price=c(tree_predicted, test_labels), 
                  type=c(replicate(length(tree_predicted), "predicted"), 
                         replicate(length(test_labels), "actual")))

ggplot(res, aes(x=price, colour=type)) +
  scale_x_continuous(labels = comma, limits = c(0, 2e+06)) +
  scale_y_continuous(labels = comma) +
  geom_density()
#Model1 Decision Tree
rpart.plot(tree_fit, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

#Model2: xgbTree Model
#trainControl()-Control the computational nuances of the train function
ctrl = trainControl(method="cv", number=10, allowParallel = TRUE)
#Combns of Vectors
param_grid <-  expand.grid(eta = c(0.3, 0.5, 0.8), 
                           max_depth = c(4:10), 
                           gamma = c(0), 
                           colsample_bytree = c(0.5, 0.6, 0.7),
                           nrounds = c(120, 140, 150, 170), 
                           min_child_weight = c(1))

#Combn chosen
param_grid <- expand.grid(eta=c(0.3), 
                          max_depth= c(6), 
                          gamma = c(0), 
                          colsample_bytree = c(0.6), 
                          nrounds = c(120),
                          min_child_weight = c(1),
                          subsample=c(0.7))
#Model 2:Fitting the predicted model
xgb_fit = train(price ~ ., 
                data=df, method="xgbTree", metric="RMSE",
                trControl=ctrl, subset = train_idx, tuneGrid=param_grid)

xgb_predicted = predict(xgb_fit, test, "raw")

summary(xgb_predicted) #predicted
summary(test_labels) #actual
cor(xgb_predicted, test_labels)
#rmse(xgb_predicted, test_labels)

#Model2: Actual v/s Predicted
res <- data.frame(price=c(xgb_predicted, test_labels), 
                  type=c(replicate(length(xgb_predicted), "predicted"), 
                         replicate(length(test_labels), "actual")))

ggplot(res, aes(x=price, colour=type)) +
  scale_x_continuous(labels = comma, limits = c(0, 2e+06)) +
  scale_y_continuous(labels = comma) +
  geom_density()

#Conclusion: XGBoost Model is more Accurate than Decision Tree Model

#Model features
#Feature Importance
imp <- varImp(xgb_fit, scale = FALSE)
print(imp)
imp_names = rev(rownames(imp$importance))
print(imp_names)
imp_vals = rev(imp$importance[, 1])
print(imp_vals)
var_importance <- data_frame(variable=imp_names,
                             importance=imp_vals)
var_importance <- arrange(var_importance, importance)
print(var_importance)
var_importance$variable <- factor(var_importance$variable, 
                                  levels=var_importance$variable)
print(var_importance$variable)

var_importance_top_15 = var_importance[with(var_importance, 
                                            order(-importance)), ][1:15, ]
print(var_importance_top_15)

ggplot(var_importance_top_15, aes(x=variable, weight=importance)) +
  geom_bar(position="dodge") + ggtitle("Feature Importance (Top 15)") +
  coord_flip() + xlab("House Attribute") + ylab("Feature Importance") +
  theme(legend.position="none")

#Comparing predicted distributions
res <- data.frame(price=c(tree_predicted, xgb_predicted, test_labels), 
                  type=c(replicate(length(tree_predicted), "tree"), 
                         replicate(length(xgb_predicted), "xgb"),
                         replicate(length(test_labels), "actual")
                  ))

ggplot(res, aes(x=price, colour=type)) +
  scale_x_continuous(labels = comma, limits = c(0,2e+06)) +
  scale_y_continuous(labels = comma) +
  geom_density()

#Difference between values of Model2 and Actual
test_sample <- sample_n(test, 10, replace=FALSE)
test_predictions <- predict(xgb_fit, test_sample, "raw")
actual_prices <- round(test_sample$price, 0)
predicted_prices <- round(test_predictions, 0)
data.frame(actual=actual_prices, 
           predicted=predicted_prices, 
           difference=actual_prices-predicted_prices)



