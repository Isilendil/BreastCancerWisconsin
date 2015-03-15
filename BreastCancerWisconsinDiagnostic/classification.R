# 2015.03.13

# classification of wdbs dataset

readData <- function()
{
  tempData <- read.table(file = 'wdbc.data', header = FALSE, sep = ',')
  colnames(tempData)[1:2] <- c('id', 'class')
  return(tempData)
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

rawData <- readData()
rawData[rawData[,'class'] == 'M', 'label'] <- 1
rawData[rawData[,'class'] == 'B', 'label'] <- 0

# Logistic Regression Algorithm
#LRClassification(trainData = rawData[1:400, 3:ncol(rawData)], testData = rawData[401:569, 3:ncol(rawData)])

# Decision Tree Algorithm
#DTClassification(trainData = rawData, testData = rawData)
#DTClassification(trainData = rawData[1:400, 3:ncol(rawData)], testData = rawData[401:569, 3:ncol(rawData)])

# Support Vector Machine Algorithm
SVMClassification(trainData = rawData[101:500, 3:ncol(rawData)], testData = rawData[501:569, 3:ncol(rawData)])





