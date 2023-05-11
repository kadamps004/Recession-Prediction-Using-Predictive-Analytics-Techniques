## Predicting Recession using Data obtained from Kaggle

#### Overview

# Loading packages and libaries that will be required for prediction
install.packages("caret")
install.packages("data.table")
library(caret)
library(e1071)
library(forecast)
library(FNN)
library(GGally)
library(data.table)

# Loading the  data to predict result from the data
prediction.df <- read.csv("/Users/priyakadam/Downloads/recession_prediction.csv", header = TRUE)  # load data

dim(prediction.df)  # find the dimension of data frame
##original data: obs = 511, var= 12

# Removing #N/A values
prediction.df$Data.Yeild.GPD <- replace(prediction.df$Data.Yeild.GPD, prediction.df$Data.Yeild.GPD == "#N/A", 0)

# printing to verify if the #N/A values are removed
print(prediction.df)



### ----------------------------------------------------------------------------
# Generating model for Linear regression

# use first 250 rows of data
prediction.df <- prediction.df[1:250, ]
# select variables for regression
selected.var <- c(6, 7, 8, 9,10,12)


# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:250), 100)  
train.df <- prediction.df[train.index, selected.var]
valid.df <- prediction.df[-train.index, selected.var]


# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
PredictRecession.lm <- lm(Data.yeild..Unemployment.Rate ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 499)
summary(PredictRecession.lm)

# Predicting validation and compute accuracy

library(forecast)
# use predict() to make predictions on a new set. 
PredictRecession.lm.pred <- predict(PredictRecession.lm, valid.df)
##other validation sets car.lm.pred <- predict(car.lm, valid2.data ) ##car.lm.pred <- predict(car.lm, newdata=car.df[valid3,],na.action=na.pass)
options(scipen=499, digits = 1)
some.residuals <- valid.df$Data.yeild..Unemployment.Rate[1:20] - PredictRecession.lm.pred[1:20]
data.frame("Predicted" = PredictRecession.lm.pred[1:20], "Actual" = valid.df$Data.yeild..Unemployment.Rate[1:20],
           "Residual" = some.residuals)

options(scipen=499, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(PredictRecession.lm.pred, valid.df$Data.yeild..Unemployment.Rate)


### ----------------------------------------------------
# Generating model for performing Niave Bais algorithm


## Performing the Niave Bias
library(e1071)
delays.nb <- naiveBayes(train.df$Data.yeild..Unemployment.Rate ~ ., data = train.df)
## predict probabilities
pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")
## predict class membership
pred.class <- predict(delays.nb, newdata = valid.df)
print(pred.class)
summary(pred.class)

accuracy <- mean(train.df$Data.yeild..Unemployment.Rate)
cat("Accuracy:", round(accuracy, 3))

### ----------------------------------------------------------------------------
## Creating Time series Model

# -------------------------------------------------------------------------


prediction.data <- read.csv("/Users/priyakadam/Downloads/recession_prediction.csv", header = TRUE)  # load data

prediction.data$Data.Yeild.GPD <- replace(prediction.data$Data.Yeild.GPD, prediction.data$Data.Yeild.GPD == "#N/A", 0)
print(prediction.data)
predictingRecession.ts <- ts(prediction.data$Date, start = c(1977, 1), end = c(2019, 12), freq = 10)

predictingRecession.ts <- as.numeric(predictingRecession.ts)

#############
## training set
library(forecast)
predictingRecession.ts <- ts(predictingRecession.ts)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)

## Computing error for the model

train.lm.poly.trend <- tslm(predictingRecession.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)

train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nValid, level = 0)


##Residual standard error: 0.156 on 8 degrees of freedom
##Multiple R-squared:  0.0632,	Adjusted R-squared:  -0.171 
##F-statistic: 0.27 on 2 and 8 DF,  p-value: 0.77

## Cheking if #N/A values exists. Since we already converted them 0, the function should a return a boolean negative
any(is.na(predictingRecession.ts)) #output is FALSE

train.lm.expo.trend.pred <- forecast(train.lm.expo.trend, h = nValid, level = 0)


# Creating a sample time series object
dates <- seq(as.Date("1977-01-01"), as.Date("2022-01-01"), by = "month")
values <- rnorm(length(dates))
predictingRecession.ts <- ts(values, start = c(1977, 1), frequency = 12)

# Subsetting the time series object
predictingRecession.ts <- window(predictingRecession.ts, start = c(1977, 1), end = c(2019, 12))


###############################################
# Plotting the time series plot
     
dates <- seq(as.Date("1977-01-01"), as.Date("2019-12-01"), by = "month")

# Convert date column to Date format
train.df$Date <- as.Date(train.df$Date)

# Create a time series object
ts_data <- ts(train.df$Data.yeild..Unemployment.Rate, start = c(1977, 1), end = c(2019, 12), frequency = 12)

# Create a time series plot
plot.ts(ts_data, main = "Unemployment Rate Time Series", xlab = "Year", ylab = "Unemployment Rate")


#END
#--------------------------------------------------

