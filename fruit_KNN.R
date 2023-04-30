library(ggplot2)
library(class)
library(caTools) 
library(GGally)

Fruits <- read.csv("fruits.csv")
View(Fruits)
Fruits$mass<- as.double(Fruits$mass)
Fruits$width<- as.double(Fruits$width)
Fruits$height<- as.double(Fruits$height)
Fruits$color_score<- as.double(Fruits$color_score)
Fruits$fruit_name<- as.factor(Fruits$fruit_name)
View(Fruits)
set.seed(123)
split = sample.split(Fruits$fruit_name,SplitRatio = 0.75)

training_set = subset(Fruits,split == TRUE) 
testing_set = subset(Fruits, split == FALSE)

View(training_set)
View(testing_set)

train_scale <- scale(training_set[, 1:4])
test_scale <- scale(testing_set[, 1:4])

classifier_knn <- knn(train = train_scale,test = test_scale,
                      cl = training_set$fruit_name,
                      k = 1)
cm <- table(testing_set$fruit_name, classifier_knn)
cm

misClassError <- mean(classifier_knn != testing_set$fruit_name)
print(paste('Accuracy of KNN with k=1 ==', 1-misClassError))

classifier_knn <- knn(train = train_scale,test = test_scale,
                      cl = training_set$fruit_name,
                      k = 9)
cm <- table(testing_set$fruit_name, classifier_knn)
cm

misClassError <- mean(classifier_knn != testing_set$fruit_name)
print(paste('Accuracy of KNN with k=9 ==', 1-misClassError))
