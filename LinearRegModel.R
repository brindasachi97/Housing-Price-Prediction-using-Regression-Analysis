library(ggplot2) # Data visualization
library(readr) # 
library(corrplot)
library(ggplot2)
df <- read.csv("/home/brinda/College /DataAnalytics/Project/kc_house_data.csv", stringsAsFactors = FALSE)
str(df)
#Removing unwanted columns
dfForCorr<-df
dfForCorr$id <- NULL
dfForCorr$date <- NULL
#removing outliers
#df<-df[!(df$price>=1000000 & df$bedrooms==0),]
# Set Seed so that same sample can be reproduced in future also
set.seed(101) 
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.80*nrow(df)), replace = F) #Good
train <- df[sample, ]
test  <- df[-sample, ]

#Initial Model
linearModel <- lm(train$price ~ (train$sqft_living+train$sqft_above+train$sqft_basement+factor(train$view)+factor(train$condition)+factor(train$floors)+factor(train$view)+ factor(train$yr_renovated)+factor(train$bedrooms)+factor(train$floors)))
linearPredicted <- predict(linearModel, test)
res <- data.frame(price=c(linearPredicted, test_labels), 
                  type=c(replicate(length(linearPredicted), "predicted"), 
                         replicate(length(test_labels), "actual")))

ggplot(res, aes(x=price, colour=type)) +
  scale_x_continuous(labels = comma, limits = c(0, 2e+06)) +
  scale_y_continuous(labels = comma) +
  geom_density()

#Model 2
#zipcode and view to factor
#columns grade,condition to ordered factor
df$zipcode <- as.factor(df$zipcode)
df$view <- as.factor(df$view)
df$grade <- as.ordered(df$grade)
df$condition <- as.ordered(df$condition)

# Set Seed so that same sample can be reproduced in future also
set.seed(101) 
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.80*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

linearMod2 <- lm(train$price ~ train$sqft_living + train$sqft_lot + train$grade + train$zipcode + train$sqft_living15 + train$sqft_above + train$sqft_basement + train$sqft_lot + train$sqft_lot15)
linearPredicted2 <- predict(linearMod2, test)
res <- data.frame(price=c(linearPredicted2, test_labels), 
                  type=c(replicate(length(linearPredicted2), "predicted"), 
                         replicate(length(test_labels), "actual")))

ggplot(res, aes(x=price, colour=type)) +
  scale_x_continuous(labels = comma, limits = c(0, 2e+06)) +
  scale_y_continuous(labels = comma) +
  geom_density()
#Model 3 adding train$view 

linearMod3 <- lm(train$price ~ train$sqft_living + train$sqft_lot + train$grade + train$zipcode + train$sqft_living15 + train$sqft_above + train$sqft_basement + train$sqft_lot + train$sqft_lot15+train$view)
linearPredicted3 <- predict(linearMod3, test)
res <- data.frame(price=c(linearPredicted3, test_labels), 
                  type=c(replicate(length(linearPredicted3), "predicted"), 
                         replicate(length(test_labels), "actual")))

ggplot(res, aes(x=price, colour=type)) +
  scale_x_continuous(labels = comma, limits = c(0, 2e+06)) +
  scale_y_continuous(labels = comma) +
  geom_density()

#ABS line Comparison

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "  Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(linearModel)
ggplotRegression(linearMod2)
ggplotRegression(linearMod3)
