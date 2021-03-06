# 2015.03.14

# classification of wpbc dataset

readData <- function()
{
  tempData <- read.table(file = 'wpbc.data', header = FALSE, sep = ',')
  colnames(tempData)[1:3] <- c('id', 'outcome', 'time')
  
  # missing value process
  # Missing attribute values: Lymph node status is missing in 4 cases.
  # Lymph node status numbered V35 is a level attribute.
  # hist(as.numeric(tempData[, 'V35']))
  # The value of V35 in around half samples is 0.
  tempData[tempData[, 'V35'] == '?', 'V35'] <- 0
  tempData[, 'V35'] = as.numeric(as.character(tempData[, 'V35']))
  
  return(tempData)
}

RecurPredictionPreprocessing <- function(data)
{
  # Positive case: Recurrence in five years
  data[, 'label'] <- 0
  data[(data[, 'outcome']=='R') & (data[, 'time']<=60), 'label'] <- 1
  return(data)
}

LRClassification <- function(trainData, testData)
{
  rate <- rep(0, nrow(trainData)-1)
  
  for(i in 2 : nrow(trainData))
  {
    glm.fit <- glm(label ~ ., data = trainData[1:i,], family = binomial)
    # summary(glm.fit)  
    # coef(glm.fit)
    # summary(glm.fit)$coef
    
    glm.probs <- predict(glm.fit, newdata = testData, type = 'response')
    rate[i-1] <- mean(testData[, 'label'] == (glm.probs > 0.5))
  }
  
  plot(rate, type = 'l')
  
}

DTClassification <- function(trainData, testData)
{
  library(rpart)
  fit <- rpart(label ~ ., data = trainData, method = 'class')
  
  plot(fit)
  text(fit)
  
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  
  fancyRpartPlot(fit)
  
  prediction <- predict(fit, newdata = testData, type = 'class')
  print(mean(testData[, 'label'] == prediction))
}

NBClassification <- function(trainData, testData)
{
  library(e1071)
  
  trainData$label <- as.factor(trainData$label)
  testData$label <- as.factor(testData$label)
  
  model <- naiveBayes(label ~ ., data = trainData, laplace = 3)
  prediction <- predict(model, newdata = testData)
  print(mean(testData[, 'label'] == prediction))
  table(prediction, testData$label)
}

SVMClassification <- function(trainData, testData)

SVMClassification <- function(trainData, testData)
{
  library(e1071)
  svm.fit <- svm(label ~ ., data = trainData)
  prediction <- ifelse(predict(svm.fit, newdata = testData) > 0, 1, 0)
  print(mean(testData[, 'label'] == prediction))
  
  svm.fit <- svm(label ~ ., data = trainData, kernel = 'linear')
  prediction <- ifelse(predict(svm.fit, newdata = testData) > 0, 1, 0)
  print(mean(testData[, 'label'] == prediction))
  
  svm.fit <- svm(label ~ ., data = trainData, kernel = 'polynomial')
  prediction <- ifelse(predict(svm.fit, newdata = testData) > 0, 1, 0)
  print(mean(testData[, 'label'] == prediction))
  
  svm.fit <- svm(label ~ ., data = trainData, kernel = 'radial')
  prediction <- ifelse(predict(svm.fit, newdata = testData) > 0, 1, 0)
  print(mean(testData[, 'label'] == prediction))
  
  svm.fit <- svm(label ~ ., data = trainData, kernel = 'sigmoid')
  prediction <- ifelse(predict(svm.fit, newdata = testData) > 0, 1, 0)
  print(mean(testData[, 'label'] == prediction))
  
#  rate <- rep(0, nrow(trainData)-1)
  
#  for(i in 2 : nrow(trainData))
#  {
#    svm.fit <- svm(label ~ ., data = trainData[1:i, 3:ncol(trainData)])
#    prediction <- ifelse(predict(svm.fit, newdata = testData) > 0, 1, 0)
#    rate[i-1] <- mean(testData[, 'label'] == prediction)
#  }
  
#  plot(rate, type = 'l')
}

###
# main function
###

setwd('~/Workspace/R/BreastCancerWisconsin/BreastCancerWisconsinPrognostic//')

rawData <- readData()

# Recurrence Prediction PreProcessing
data <- RecurPredictionPreprocessing(rawData)

cutPoint <- 170

trainNumber <- 150

set.seed(0)
trainRow <- sample(nrow(data), trainNumber)

train <- data[trainRow, 4:ncol(data)]
test <- data[-trainRow, 4:ncol(data)]

# Logistic Regression Algorithm
#LRClassification(trainData = data[1:cutPoint, 4:ncol(data)], testData = data[(cutPoint+1):(nrow(data)), 4:ncol(data)])
#LRClassification(trainData = data[1:cutPoint, 4:ncol(data)], testData = data[(cutPoint+1):(nrow(data)), 4:ncol(data)])
LRClassification(trainData = train, testData = test)

# Decision Tree Algorithm
#DTClassification(trainData = rawData, testData = rawData)
#DTClassification(trainData = data[1:cutPoint, 4:ncol(data)], testData = data[(cutPoint+1):(nrow(data)), 4:ncol(data)])
DTClassification(trainData = train, testData = test)

# Naive Bayes Algorithm
#NBClassification(trainData = data[, 4:ncol(data)], testData = data[, 4:ncol(data)])
NBClassification(trainData = train, testData = test)
#NBClassification(trainData = data[1:cutPoint, 4:ncol(data)], testData = data[(cutPoint+1):(nrow(data)), 4:ncol(data)])

# Support Vector Machine Algorithm
SVMClassification(trainData = train, testData = test)





