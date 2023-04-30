library(ggplot2)
library(caTools) 
library(GGally) 
library(e1071)

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

ggpairs(training_set,ggplot2::aes(colour = fruit_name,alpha=0.6))

#SVC
class1 = svm(fruit_name~.,data= training_set,type = "C-classification",
             kernal = "radial")

pred1 = predict(class1, type ='response',newdata = testing_set)
comp = data.frame(testing_set$fruit_name,pred1)

View(comp)

cm1 = table(testing_set[,5],pred1) 
cm1

misClassError <- mean(pred1 != testing_set$fruit_name)
print(paste('Accuracy of SVM =', 1-misClassError))
