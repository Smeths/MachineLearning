rm(list = ls())
library(ggplot2)
library(grid)
library(gridExtra)
library(caret)

# loading testing and training data
trainfile <- "training.csv"
data <- read.csv(trainfile)
testfile <- "testing.csv"
testing <- read.csv(testfile)

# Finding variables used in the test data and extracting modelling variables
keep <- as.vector(apply(testing,2,function(x) sum(is.na(x)) != 20))
keep[1:7] <- FALSE
testing <- testing[,keep]

# Keeping the same data in the training set and centering and scaling
keep[160] = TRUE
data <- data[,keep]

# splitting into training and validation set
intrain <- createDataPartition(y=data$classe,p=0.6, list=FALSE)
training <- data[intrain,]
valid <- data[-intrain,]

# Scaling training data
scaleObj <- preProcess(training[,-53],method=c("center","scale"))
straining <- predict(scaleObj,training[,-53])
straining$classe <- training$classe

# Scaling the validation set using the same scale object
svalid <- predict(scaleObj,valid[,-53])
svalid$classe <- valid$classe

# Scaling the test set using the same scale object
stesting <- predict(scaleObj,testing[,-53])
stesting$classe <- testing$classe

#####################
# Fitting lda model #
#####################

modlda <- train(classe ~ ., method="lda",data=straining,verbose=FALSE)
modlda

# Assessing out of sample error
predvallda <- predict(modlda,svalid)
confusionMatrix(svalid$classe,predvallda)

# Predicting using test data
predlda <- predict(modlda,stesting)
predlda

####################
# Fitting rf model #
####################

# singular value decomposition
svd <-  svd(straining[,-53])
plot(svd$d^2/sum(svd$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

# As random forest can be slow I shall preprocess using a PCA analysis and
# keep only the first 5 principle components

# Preprocessing data
preProc <- preProcess(straining[,-53],method="pca",pcaComp=15)
strainPC <- predict(preProc,straining[,-53])
strainPC$classe <- straining[,53]

modrf <- train(classe ~ .,method="rf",data=strainPC)
modrf

# Assessing out of sample error
svalidPC <-predict(preProc,svalid)
predvalrf <- predict(modrf,svalidPC)
confusionMatrix(svalid$classe,predvalrf)

# Predicting using test data
testrfPC <-predict(preProc,stesting)
predrf <- predict(modrf,testrfPC)
predrf

# Finding percentage of times model predictions coincide

sum(predvallda==predvalrf)/length(predvallda)







