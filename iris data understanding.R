#####################
#Loading Iris dataset
#####################

# Method 1
library(datasets)
data(iris)

iris <- datasets::iris

# Method 2
# install.packages("Rcurl")

library(Rcurl)
iris <- read.csv(text =getURL("https://raw.githubusercontent.com/aastha2403/codes/main/iris.csv"))

# view the data
View(iris)

############################
# Display summary statistics
############################

# head/tail
head(iris)
tail(iris)

#to get first 5
head(iris, 5)
tail(iris, 5)

#to get the summary
summary(iris)
summary(iris$Sepal.Length)

# check to see if there is missing data
sum(is.na(iris))

# expands summary by providing larger set of statistics
#install.packages("skimr")

library(skimr)

#perform skim to display summary statistics
skim(iris)

# group data by species then perform skim
iris %>%
  dplyr::group_by(Species) %>%
  skim()

##########################
# Quick data visualization
##########################

#Panel plots
plot(iris)
plot(iris,col= 'red')

#Scatter plots
plot(iris$Sepal.Width, iris$Sepal.Length)
plot(iris$Sepal.Width, iris$Sepal.Length, col= 'red')
plot(iris$Sepal.Width, iris$Sepal.Length, col = 'red',
     xlab ="Sepal Width", ylab ="Sepal Length")

#histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col= 'blue')
hist(iris$Sepal.Width, col= 'blue', xlab= "Sepal Width")

#Feature plots
library(caret)
featurePlot(x = iris[,1:4],
            y = iris$Species,
            plot = "box",
            strip= strip.custom(par.strip.text = list(cex=.7)),
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")))

################################
#Building a Classification Model
################################

#to achieve reproducible model; set a random seed number
set.seed(100)

#perform stratified random split of the data set
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list=FALSE)
TrainingSet <- iris[TrainingIndex,]
TestingSet <- iris[-TrainingIndex,]

plot(TrainingSet, TestingSet, col='red')

#############################
#SVM model(polynomial kernel)
#############################

# Build a training model
Model <- train(Species ~., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess= c("scale","center"),
               trControl = trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
               )

# Build CV model
Model.cv <- train(Species ~., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess= c("scale","center"),
               trControl = trainControl(method="cv", number=10),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
               )

###########################
#Apply model for prediction
###########################

# Apply model to make prediction on training set
Model.training <- predict(Model, TrainingSet)

# Apply model to make prediction on testing set
Model.testing <- predict(Model, TestingSet)

#Perform cross validation
Model.cv <- predict(Model.cv, TrainingSet)

#Model performance
Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$Species);Model.training.confusion
Model.testing.confusion <- confusionMatrix(Model.testing, TestingSet$Species);Model.testing.confusion
Model.cv.confusion <- confusionMatrix(Model.cv, TrainingSet$Species);Model.cv.confusion

#Feature Importance
Importance <- varImp(Model);Importance
plot(Importance, col ='Purple')

