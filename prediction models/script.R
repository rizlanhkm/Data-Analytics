rm(list = ls())
Phish <- read.csv("PhishingData.csv")
set.seed(33550166)
L <- as.data.frame(c(1:50))
L <- L[sample(nrow(L), 10, replace = FALSE),]
Phish <- Phish[(Phish$A01 %in% L),]
pd <- Phish[sample(nrow(Phish), 2000, replace = FALSE),] # sample of 2000 rows

attach(pd)

# 1. Explore the data: What is the proportion of phishing sites to legitimate sites? Obtain
# descriptions of the predictor (independent) variables – mean, standard deviations, etc.
# for real-valued attributes. Is there anything noteworthy in the data? Are there any
# attributes you need to consider omitting from your analysis?

pd0 = pd[pd$Class == "0",]
pd1 = pd[pd$Class == "1",]

rows = c(nrow(pd0), nrow(pd1))
percentages = round(rows/sum(rows)*100)
labels = c("Phishing", "Legitimate")
percentages = paste(percentages,"%", sep = "")

colors = c("orange", "cadetblue1")

pie(rows, 
    labels = percentages, 
    main = "Proportion of phishing sites to legitimate sites", 
    col = colors)
legend("topright", labels, cex = 0.8, fill = colors)
ratio = nrow(pd0)/nrow(pd1)
ratio


summary(pd)
str(pd)

# 2. Document any pre-processing required to make the data set suitable for the model fitting
# that follows.

## remove columns with no varying values
# to.drop = c("A03")
# pd = pd[, !names(pd) %in% to.drop]

#omit NA values
pd = na.omit(pd)

# change Class column to be factors instead of int to make it categorical
pd$Class = as.factor(pd$Class)
str(pd)


# 3. Divide your data into a 70% training and 30% test set.

set.seed(33550166) # Student ID as random seed
train.row = sample(1:nrow(pd), 0.7*nrow(pd))
pd.train = pd[train.row,]
pd.test = pd[-train.row,]


# 4. Implement a classification model using each of the following techniques

library(tree) 
library(e1071) 
library(ROCR) 
library(rpart)
library(adabag) 
library(randomForest)

################################ Decision Tree #################################
pd.tree = tree(Class~. , data = pd.train)
summary(pd.tree)

plot(pd.tree)
text(pd.tree, pretty = 0)

################################# Naive Bayes ##################################
pd.bayes = naiveBayes(Class~., data = pd.train)

################################### Bagging ####################################
pd.bag = bagging(Class ~ ., data = pd.train)


################################## Boosting ####################################
pd.boost = boosting(Class ~ ., data = pd.train)


################################ Random Forest #################################
pd.rf = randomForest(Class ~ ., data = pd.train, na.action = na.exclude)


#------------------------------------------------------------------------------#
# 5. Using the test data, classify each of the test cases as ‘phishing (1)’ or ‘legitimate (0)’.
# Create a confusion matrix and report the accuracy of each model.

################################ Decision Tree #################################
pd.pred = predict(pd.tree, pd.test, type = "class")

# confusion matrix
t.tree = table(predicted = pd.pred, actual = pd.test$Class)

dt.acc = sum(diag(t.tree))/sum(t.tree)

t.tree
dt.acc

################################# Naive Bayes ##################################
pd.predbayes = predict(pd.bayes, pd.test)

t.bayes = table(predicted = pd.predbayes, actual = pd.test$Class)

bayes.acc = sum(diag(t.bayes))/sum(t.bayes)

t.bayes
bayes.acc

################################### Bagging ####################################
pd.predbag = predict.bagging(pd.bag, pd.test)

bag.acc = sum(diag(pd.predbag$confusion))/sum(pd.predbag$confusion)

# built-in confusion matrix
pd.predbag$confusion
bag.acc

################################## Boosting ####################################
pd.predboost = predict.boosting(pd.boost, pd.test)

boost.acc = sum(diag(pd.predboost$confusion))/sum(pd.predboost$confusion)

# built-in confusion matrix
pd.predboost$confusion
boost.acc

################################ Random Forest #################################
pd.predrf = predict(pd.rf, pd.test)

t.rforest = table(predicted = pd.predrf, actual = pd.test$Class)

rf.acc = sum(diag(t.rforest))/sum(t.rforest)

t.rforest
rf.acc

#------------------------------------------------------------------------------#
# 6. Using the test data, calculate the confidence of predicting ‘phishing’ for each case and
# construct an ROC curve for each classifier. Calculate the AUC for each classifier.

################################ Decision Tree #################################
# make vector 
pd.predtree.vector = predict(pd.tree, pd.test, type = "vector")

# ROC grpah
pddtPred = ROCR::prediction(pd.predtree.vector[,2], pd.test$Class)
pddtPerf = performance(pddtPred, "tpr", "fpr")
plot(pddtPerf, col = "plum4", main = "ROC curve for every classifier model")
abline(0,1)

roc.labels = c("Decision Tree")
roc.colors = c("plum4")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.tree = performance(pddtPred, "auc")
auc.tree.val=as.numeric(auc.tree@y.values)

################################# Naive Bayes ##################################
# prediction model
pd.predbayes.raw = predict(pd.bayes, pd.test, type = "raw")
pdbPred = ROCR::prediction(pd.predbayes.raw[,2], pd.test$Class)
pdbPerf = performance(pdbPred, "tpr", "fpr")

plot(pdbPerf, add = TRUE, col = "violet")

roc.labels = c(roc.labels, "Naive Bayes")
roc.colors = c(roc.colors, "violet")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.bayes = performance(pdbPred, "auc")
auc.bayes.val=as.numeric(auc.bayes@y.values)

################################### Bagging ####################################
# create prediction object
pdbagPred = ROCR::prediction(pd.predbag$prob[,2], pd.test$Class)

# find ROC curve
pdbagPerf = performance(pdbagPred, "tpr", "fpr")

plot(pdbagPerf, add = TRUE, col = "red")

roc.labels = c(roc.labels, "Bagging")
roc.colors = c(roc.colors, "red")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.bag = performance(pdbagPred, "auc")
auc.bag.val=as.numeric(auc.bag@y.values)

################################## Boosting ####################################
pdboostPred = ROCR::prediction(pd.predboost$prob[,2], pd.test$Class)
pdboostPerf = performance(pdboostPred, "tpr", "fpr")

plot(pdboostPerf, add = TRUE, col = "green")

roc.labels = c(roc.labels, "Boosting")
roc.colors = c(roc.colors, "green")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.boost = performance(pdboostPred, "auc")
auc.boost.val=as.numeric(auc.boost@y.values)

################################ Random Forest #################################
pd.predrf = predict(pd.rf, pd.test, type = "prob")
pdrfPred = ROCR::prediction(pd.predrf[,2], pd.test$Class)
pdrfPerf = performance(pdrfPred, "tpr", "fpr")

plot(pdrfPerf, add = TRUE, col = "blue")

roc.labels = c(roc.labels, "Random Forest")
roc.colors = c(roc.colors, "blue")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.rf = performance(pdrfPred, "auc")
auc.rf.val=as.numeric(auc.rf@y.values)


# 7. Create a table comparing the results in Questions 5 and 6 for all classifiers. Is there a
# single “best” classifier?

compare.classifiers.acc = data.frame(
  Classifiers = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),
  Accuracy = c(dt.acc, bayes.acc, bag.acc, boost.acc, rf.acc)
)

compare.classifiers.auc = data.frame(
  Classifiers = compare.classifiers.acc$Classifiers,
  AUC = c(auc.tree.val, auc.bayes.val, auc.bag.val, auc.boost.val, auc.rf.val)
)

# sort the accuracy values in descending order
compare.classifiers.acc = compare.classifiers.acc[order(-compare.classifiers.acc$Accuracy),]

# sort the auc values in descending order
compare.classifiers.auc = compare.classifiers.auc[order(-compare.classifiers.auc$AUC),]

compare.classifiers.acc
compare.classifiers.auc


# 8. Examining each of the models, determine the most important variables in predicting
# whether a web site will be phishing or legitimate. Which variables could be omitted from
# the data with very little effect on performance?
summary(pd.tree)
summary(pd.bayes)
head(sort(-pd.bag$importance))
head(sort(-pd.boost$importance))
head(order(-pd.rf$importance))
# Most important are A23, A01 and A18

sort(pd.bag$importance)
sort(pd.boost$importance)
order(pd.rf$importance)
# A03, A05, A07, A13, A25

# 9. Starting with one of the classifiers you created in Question 4, create a classifier that is
# simple enough for a person to be able to classify whether a site is phishing or legitimate
# by hand. Describe your model with either a diagram or written explanation. What factors
# were important in your decision? State why you chose the attributes you used. Using the
# test data created in Question 3, evaluate model performance using the measures you
# calculated for Questions 5 and 6. How does it compare to those in Question 4?

pd.simpletree = tree(Class ~ A23, data = pd.train)

plot(pd.simpletree)
text(pd.simpletree, pretty = 0)

# evaluate model performance
pdtSimple.pred = predict(pd.simpletree, pd.test, type = "class")

# confusion matrix
t.simple = table(predicted = pdtSimple.pred, actual = pd.test$Class)

# accuracy
dtsimple.acc = sum(diag(t.simple))/sum(t.simple)

t.simple
dtsimple.acc

# make vector 
pdtSimple.pred.vector = predict(pd.simpletree, pd.test, type = "vector")

# ROC grpah
pdt.simplePred = ROCR::prediction(pdtSimple.pred.vector[,2], pd.test$Class)
pddtPerfsimple = performance(pdt.simplePred, "tpr", "fpr")
plot(pddtPerfsimple, col = "goldenrod", add = TRUE)

# add to legends
roc.labels = c(roc.labels, "Simple Decision Tree")
roc.colors = c(roc.colors, "goldenrod")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.tree.simple = performance(pdt.simplePred, "auc")
auc.tree.simple.val=as.numeric(auc.tree.simple@y.values)
auc.tree.simple.val


# compare only the decision trees
plot(pddtPerf, col = "plum4", main = "ROC curve for Desicion Trees")
plot(pddtPerfsimple, col = "goldenrod", add = TRUE)
abline(0,1)

roc.labels = c("Decision Tree", "Simple Decision Tree")
roc.colors = c("plum4", "goldenrod")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)


# 10. Create the best tree-based classifier you can. You may do this by adjusting the
# parameters, and/or cross-validation of the basic models in Question 4. Show that your
# model is better than the others using the measures you calculated for Questions 5 and 6.

library(caret)


control = trainControl(method = "cv", number = 10)

tree.train = train(Class ~ ., data = pd.train, method = "cforest", trControl = control)
summary(tree.train)


# calculate accuracy
tree.pred = predict(tree.train, pd.test, type = "raw")
t.tree.improved = table(predicted = tree.pred, actual = pd.test$Class)
tree.improved.acc = sum(diag(t.rforest))/sum(t.rforest)

t.tree.improved
tree.improved.acc


# evaluate model performance
tree.pred = predict(tree.train, pd.test, type = "prob")
tree.pred.rf = ROCR::prediction(tree.pred[,2], pd.test$Class)
tree.perf.rf = performance(tree.pred.rf, "tpr", "fpr")

# plot ROC Curve
plot(tree.perf.rf, add = TRUE, col = "lightsteelblue")

roc.labels = c(roc.labels, "Improved Random Forest")
roc.colors = c(roc.colors, "lightsteelblue")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.rf.improved = performance(tree.pred.rf, "auc")
auc.rf.improved.val=as.numeric(auc.rf.improved@y.values)
auc.rf.improved.val





# 12. Fit a new classifier to the data, test and report its performance in the same way as for
# previous models. You can choose a new type of classifier not covered in the course, or a
# new version of any of the classifiers we have studied.

# turn Class back into a factor
pd.train$Class = as.factor(pd.train$Class)
pd.test$Class = as.factor(pd.test$Class)

# https://www.rdocumentation.org/packages/e1071/versions/1.7-14/topics/svm

# create a prediciton model with Support Vector Machine from the e1071 library
pd.svm = svm(Class ~ ., data = pd.train, scale = FALSE, cost = 5, probability = TRUE)
summary(pd.svm)

pd.predsvm = predict(pd.svm, pd.test, probability = TRUE)

# get only the probabilities attribute
pd.predsvm.probs = attr(pd.predsvm, "probabilities")[,2]

# confusion matrix
t.svm = table(predicted = pd.predsvm, actual = pd.test$Class)
t.svm

# calculate accuracy
svm.acc = sum(diag(t.svm))/sum(t.svm)
svm.acc

# ROC graph
pd.svm.pred = ROCR::prediction(pd.predsvm.probs, pd.test$Class)

pd.svm.perf = performance(pd.svm.pred, "tpr", "fpr")

plot(pd.svm.perf, add = TRUE, col = "orange")

roc.labels = c(roc.labels, "SVM")
roc.colors = c(roc.colors, "orange")
legend("bottomright", roc.labels, cex = 0.6, fill = roc.colors)

# calculate AUC
auc.svm = performance(pd.svm.pred, "auc")
auc.svm.val=as.numeric(auc.svm@y.values)
auc.svm.val






# 11. Using the insights from your analysis so far, implement an Artificial Neural Network
# classifier and report its performance.

library(neuralnet)

options(digit = 4)


# pre processsing
# create a vector of rows to use for the model
columns.to.use = c(1, 8, 12, 18, 22, 23, 24, 26)

# change class column into numeric
pd.train$Class = as.numeric(pd.train$Class)
pd.test$Class = as.numeric(pd.test$Class)

# create a subset of the dataframes which only contain the specified columns
pd.neural = pd[, columns.to.use]
pd.train.neural = pd.train[, columns.to.use]
pd.test.neural = pd.test[, columns.to.use]


attach(pd.neural)

set.seed(33550166)

# fit model
pd.nn = neuralnet(Class ~ A01 + A08 + A12 + A18 + A22 + A23 + A24,
                  hidden = c(7,5),
                  data = pd.train.neural,
                  linear.output = FALSE)

summary(pd.nn)

# make predictions
pd.nn.pred = compute(pd.nn, pd.test)

prob = pd.nn.pred$net.result
pred = ifelse(prob>0.5, 1, 0) 

pd.nn.pred.table = table(observed = pd.test$Class, predicted = pred)

# Calculate accuracy
pd.nn.accuracy <- sum(diag(pd.nn.pred.table)) / sum(pd.nn.pred.table)

pd.nn.pred.table
pd.nn.accuracy

pd.nn.pred

# create raw predictions
neural.train.pred = predict(pd.nn, pd.test, type = "raw")
neural.train.pred

# ROC
neural.pred = ROCR::prediction(neural.train.pred, pd.test$Class)
neural.perf = performance(neural.pred, "tpr", "fpr")

plot(neural.perf, col = "olivedrab", main = "ROC curve of Artificial Neural Network model")
abline(0,1)

#AUC
neural.auc = performance(neural.pred, "auc")
neural.auc@y.values

# model with 100 neurons in the first hidden layer (improved performance)
pd.nn = neuralnet(Class ~ A01 + A08 + A12 + A18 + A22 + A23 + A24,
                  hidden = 100,
                  data = pd.train.neural,
                  linear.output = FALSE)


