#####################
#Loading dhfr dataset
#####################

# Method 1
library(datasets)
data("dhfr")

# view the data
View(dhfr)

############################
# Display summary statistics
############################

# head/tail
head(dhfr)
tail(dhfr)

#to get first 5
head(dhfr, 5)
tail(dhfr, 5)

#to get the summary
summary(dhfr)
summary(dhfr$Y)

# check to see if there is missing data
sum(is.na(dhfr))

# expands summary by providing larger set of statistics
#install.packages("skimr")

library(skimr)

#perform skim to display summary statistics
skim(dhfr)

# group data by Y (biological activity) then perform skim
dhfr %>%
  dplyr::group_by(Y) %>%
  skim()

##########################
# Quick data visualization
##########################

#Panel plots
#plot(dhfr)
#plot(dhfr,col= 'red')

#Scatter plots
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol)
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col= dhfr$Y)
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = 'red',
     xlab ="zagreb", ylab ="weinerPol")

#histogram
hist(dhfr$moe2D_zagreb)
hist(dhfr$moe2D_zagreb, col= 'blue')
hist(dhfr$moe2D_zagreb, col= 'blue', xlab= "zagreb")

#Feature plots
library(caret)
featurePlot(x = dhfr[,2:5],
            y = dhfr$Y,
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
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list=FALSE)
TrainingSet <- dhfr[TrainingIndex,]
TestingSet <- dhfr[-TrainingIndex,]


#############################
#SVM model(polynomial kernel)
#############################

# Build a training model
Model <- train(Y ~., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess= c("scale","center"),
               trControl = trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Y ~., data = TrainingSet,
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
Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$Y);Model.training.confusion
Model.testing.confusion <- confusionMatrix(Model.testing, TestingSet$Y);Model.testing.confusion
Model.cv.confusion <- confusionMatrix(Model.cv, TrainingSet$Y);Model.cv.confusion

#Feature Importance
Importance <- varImp(Model)
plot(Importance, top= 25)
plot(Importance, top= 25, col='red')
