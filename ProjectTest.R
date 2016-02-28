
require(randomForest)
require(caret)

urlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(urlTrain), na.strings=c("NA","#DIV/0!",""))
testing  <- read.csv(url(urlTest), na.strings=c("NA","#DIV/0!",""))

dim(testing);dim(training)

training <- training[, colSums(is.na(training)) == 0]
testing  <- testing[, colSums(is.na(testing)) == 0] 
ncol(training) == ncol(testing)               

training     <- training[, -(1:6)]
testing      <- testing[, -(1:6)]

inTrain      <- createDataPartition(training$classe, p=0.7, list=FALSE)

inTraining   <- training[inTrain,] 

inValidation <- training[-inTrain,]    

class(training$classe)



#pcaTraining   <- preProcess(inTraining[,-48], method = "pca", thresh=".95")
#pcaValidation <- preProcess(inValidation[,-48],method ="pca", thresh=".95")

ctrl <- trainControl(method ="cv", preProcOptions ="pca")
modelFit_rf <- train(inTraining$classe~., method = "rf", data=inTraining, trControl = ctrl)



####PreprocessingPrincipal Components Analysis





####Machine Learning Model for prediction

#####Random Forest (rf)

#####Generalized Linear Model (glm)


####Analysis of predicted ResultsOut-of-sample error - missclassified observations/total observations



#ConfusionMatrix


# In SVM, for example, the ratio data in the support vector to the number of data is an in-sample estimation of error of the model.




class(training$classe)

# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)



* Apply preProcess function to inTraining & inValidation data sets (cross validation).
* *Note that all cleaning and preProcessing algorithms must also be applied to the test data set.

+ pcaTraining   <- preProcess(inTraining[,-48], method = "pca", thresh=".99")
+ pcaValidation <- preProcess(inValidation[,-48],method ="pca"thresh=".99")
```{r}
pcaTraining   <- preProcess(inTraining[,-48], method = "pca", thresh=".99")
pcaValidation <- preProcess(inValidation[,-48],method ="pca", thresh=".99")
```







