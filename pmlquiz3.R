library(AppliedPredictiveModeling)
data(segmentationOriginal) 
library(caret)
library(dplyr)
library(ggplot2)

# question 1

testing<-segmentationOriginal %>%
  select(everything()) %>% 
  filter(Case=="Test")
training<-segmentationOriginal %>%
  select(everything()) %>% 
  filter(Case=="Train")
dim(testing); dim(training)
options(scipen=999)
training %>% 
    ggplot(aes(x=TotalIntenCh2,y=FiberWidthCh1, colour=Class)) +
geom_point()


set.seed(125)
# using train() caret function to predict Class, using all the features available in the training dataset using rpart (regression and classification trees package)
FitModel <- train(Class ~ ., method = "rpart", data = training)
print(FitModel$finalModel)

suppressMessages(library(rattle))
library(rpart.plot)
fancyRpartPlot(FitModel$finalModel)
