# 2015.3.19

# classification of wpbc dataset 
# take label in wdbc dataset as the extra feature 

readWdbc <- function()
{
  tempData <- read.table(file = 'wdbc.data', header = FALSE, sep = ',')
  colnames(tempData) <- c('id', 'class', columnNamesMean, columnNamesSE, columnNamesWorst)
  return(tempData)
}

readWpbc <- function()
{
  tempData <- read.table(file = 'wpbc.data', header = FALSE, sep = ',')
  colnames(tempData) <- c('id', 'outcome', 'time', columnNamesMean, columnNamesSE, columnNamesWorst, 'TumorSize', 'LymphNodeStatus')
  
  # missing value process
  # Missing attribute values: Lymph node status is missing in 4 cases.
  # Lymph node status numbered 35 is a level attribute.
  # hist(as.numeric(tempData[, 'LymphNodeStatus']))
  # The value of V35 is around halp samples is 0.
  tempData[tempData[, 'LymphNodeStatus'] == '?', 'LymphNodeStatus'] <- 0
  tempData[, 'LymphNodeStatus'] = as.numeric(as.character(tempData[, 'LymphNodeStatus']))
  
  return(tempData)
}

RecurPredictionPreprocessing <- function(data)
{
  # Positive case: Recur in five years
  data[, 'label'] <- 0
  data[(data$outcome == 'R') & (data$time <= 24), 'label'] <- 1
  return(data)
}

LRTrain <- function(trainData)
{
  glm.fit <- glm(label ~ ., data = trainData, family = binomial)
  return(glm.fit)
}

LRPredict <- function(model, testData)
{
  glm.probs <- predict(model, newdata = testData, type = 'response')
  print('accuracy:')
  accuracy <- mean(testData$label == (glm.probs > 0.5))
  print(accuracy)
}

AddFeature <- function(model, data)
{
  glm.probs <- predict(model, newdata = data, type = 'response')
  data$extra <- 0
  data$extra[(glm.probs > 0.5)] <- 1
  return(data)
}

DTTrain <- function(trainData)
{
  library(rpart)
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  
  model <- rpart(label ~ ., data = trainData, method = 'class')
  
  fancyRpartPlot(model)
  
  return(model)
}

DTPredict <- function(model, data)
{
  prediction <- predict(model, newdata = data, type = 'class')
  print('accuracy:')
  accuracy <- mean(data$label == prediction)
  print(accuracy)
}

# main 

setwd('~/Workspace/R/BreastCancerWisconsin/BreastCancerWisconsin/FeatureAdded/')

columnNamesMean <- c('ma', 'mb', 'mc', 'md', 'me', 'mf', 'mg', 'mh', 'mi', 'mj')
columnNamesSE <- c('sa', 'sb', 'sc', 'sd', 'se', 'sf', 'sg', 'sh', 'si', 'sj')
columnNamesWorst <- c('wa', 'wb', 'wc', 'wd', 'we', 'wf', 'wg', 'wh', 'wi', 'wj')
  
wdbc <- readWdbc()
wdbc[wdbc$class == 'M', 'label'] <- 1
wdbc[wdbc$class == 'B', 'label'] <- 0
  
wpbc <- readWpbc()
wpbc <- RecurPredictionPreprocessing(wpbc)

trainNumberInWdbc <- 300

set.seed(0)
trainRowInWdbc <- sample(nrow(wdbc), trainNumberInWdbc)
trainWdbc <- wdbc[trainRowInWdbc, 3:ncol(wdbc)]
testWdbc <- wdbc[-trainRowInWdbc, 3:ncol(wdbc)]

modelOnWdbc <- LRTrain(trainWdbc)
LRPredict(modelOnWdbc, testWdbc)

wpbc <- AddFeature(modelOnWdbc, wpbc)

trainNumberInWpbc <- 100

set.seed(10)
trainRowInWpbc <- sample(nrow(wpbc), trainNumberInWpbc)
trainWpbc <- wpbc[trainRowInWpbc, 4:ncol(wpbc)]
testWpbc <- wpbc[-trainRowInWpbc, 4:ncol(wpbc)]

modelOnWpbc0 <- LRTrain(trainWpbc[, 1:(ncol(trainWpbc)-1)])
LRPredict(modelOnWpbc0, testWpbc)

dt0 <- DTTrain(trainWpbc[, 1:(ncol(trainWpbc)-1)])
DTPredict(dt0, testWpbc)

modelOnWpbc <- LRTrain(trainWpbc)
LRPredict(modelOnWpbc, testWpbc)

dt <- DTTrain(trainWpbc)
DTPredict(dt, testWpbc)









